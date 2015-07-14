import ForSyDe.Shallow
import ForSyDe.Shallow.Patterns

-- One basic principe may stand as this work's manifesto: there is no "cleverness" in designing a ForSyDe
-- system. One should not employ fancy tricks, rather express the functionality in terms of a structured
-- network of processes. Thus a network should clearly state "what it does" rather than "how to imple-
-- ments it".

-- 2015.06.14
-- 'histogramSY' is one good way of describing a histogram on SY signals. It is by far a terrible 
-- implementation when it comes to efficiency in execution but it exposes the structure of the network
-- and some interesting properties that can be further analyzed.
--
-- First of all: "what does it do?". A histogram accumulates the occurences of a specific pixel value in
-- a vector with L elements (where L = possible pixel levels). OK, so we need a vector with 256 elements
-- where its index corresponds to pixel levels...
--- * vector [0..255]: makes sense now. It creates a vector of indexes.
--- * Process <$> Vector <*> Vector: is obviously a farm1 pattern. Since the application operand is 
---   implemented with zipWithSY, the pattern dimension (length) will be given by the previous vector of 
---   indexes, i.e. 256. This can be determined by a model analizer. 
--- * fanoutPN img: Distributes img to the "threads" downstream. Check the documentation for 'fanoutPN'
---   to understand why I am doing this. Since this project mainly targets GPUs, this is pretty much 
---   telling us that img will reside in the global memory. But then again, this thought should be erased
---   from our minds since it may corrupt the way we are thinking in terms of "what and not how"
--- * hist: is a composite process applied through <$> on the newly-created "vector" of img. Judging by
---   what I said earlier, this means that for the same input image, we are creating 256 hist processes,
---   one for each pixel level, apply a mask which hides pixels that do not have the current designated 
---   value, and count the pixels which passed through a reduction network.
---
-- The 'histogramSY' network tells me which functionality I need to achieve. Based on that, we can intuit
-- a possible implementation on a GPU:
--- * img will surely lie in the global memory (due to fanout)
--- * the result histogram will also probably lie in a global memory or another hierarchical zone with 
---   easy access
--- * systematic masking and counting ad literam implies a terribly inefficient process, although correct. 
---   A transformation towards the implementation domain is necessary in order to translate this into some 
---   sort of mapping. 
histogramSY     :: (Num a, Eq a, Enum a) => Vector (Signal a) -> Vector (Signal Int)
histogramSY img = hist <$> vector [0..255] <*> fanoutPN img
  where 
    hist i        = reduPN (zipWithSY (+)) . farmPN (mapSY counters) . maskPN (==i)
    counters Abst = 0
    counters _    = 1 


-- 2015.06.15
-- "histogramSY'" is yet another process network model for computing the histogram of an image, but this
-- one "feels" a bit more like a memory-based architecture implementation. For starters, I am not distri-
-- buting the image to multiple workers, rather I KNOW it is a single entity somewhere. Also, the output
-- suggests that there is also another more-or-less contiguous location that stores the results...
--- * maphist is mapped throuh a farm pattern to every pixel of the image. Its role is to generate a 
---   vector with 256 elemets, where the n-th element (=pixel value) is 1 and the rest are 0.
--- * the result is basically obtained by sum-overlapping these vectors through a nice reduction pattern
---   thus clearly suggesting a map-sum operation, typically employed in both C and CUDA programs.
--
-- This description, as compared to the previous one is more implementation-oriented and looks like
-- an optimization hack rather than a system that "gives me an ordered vector with the elements correspon-
-- ding to the number of occurences of each pixel value" i.e. "the n-th element belonging to the sum of
-- pixels with value n"
-- 
-- Most likely future revisions of histogramSY and histogramSY' will look a bit differently, e.g. a more
-- straighforward description of maphist. Defining transformation rules between histogramSY and histogramSY'
-- will have high priority, and also defining ways of expressing them in order to facilitate these 
-- transformations.
histogramSY' :: Vector (Signal Int) -> Vector (Signal Int)
histogramSY' = unzipxPN . reduPN (zipWithSY sumhist) . farmPN (mapSY maphist)
  where
    maphist px = replaceV (copyV 256 0) px 1
    sumhist    = zipWithV (+)
