var jekyllBootstrapDoc = {
    buildSideMenu: function() {
	var html = '';
	// console.log("HALLO!!");
	    
	$('#main_content').each(function() {
	    var h123 = $(this).find('h1[id], h2[id], h3[id]:not([data-no-menu])');

	    var sections = [];
	    h123.each(function(ix, elem) {
		if(elem.nodeName == "H1")
		    sections.push([$(this),[]]);
		else
		    if (sections.length)
			sections[sections.length - 1][1].push($(this));
	    })	    
	    // console.log("Inside!!");
	    // console.log("init: %o", h123);
	    // console.log("split: %o", sections);

	    sections.forEach(function(section) {
	    	// console.log("section: %o", section[0][0]);
	    	// console.log("content: %o", section[1]);

		html += '<li><a href="#' + section[0][0].id + '">' + section[0].clone().children().remove().end().text() + '</a>';

		if (section[1].length){
		    html += '<ul class="nav">';
	    	    section[1].forEach(function(subsection) {
	    		// console.log("subsect: %o", subsection[0]);
	    		html += '<li><a href="#' + subsection[0].id + '">' + subsection.clone().children().remove().end().text() + '</a></li>';	
	    	    });
	    	    html += '</ul>';
		}
		html += '</li>';
	    })
	});

	if (html == '') {
	    // $('[role=complementary]').hide();
	    // $('[role=main]').removeClass('col-md-9').addClass('col-md-12');
	}
	else {
	    $('[role=complementary]').show();
	    $('[role=main]').removeClass('col-md-12').addClass('col-md-9');
	    $('.bs-docs-sidenav').html(html);
	}
    },
    
    addHeadingAnchors: function() {
	$('h1[id], h2[id], h3[id], h4[id], h5[id]').each(function() {
	    if (this.id != "project_title" && this.id != "project_tagline")
	    if ($(this).children('.anchor-link').length === 0) {
		$(this).prepend('<a href="#' + this.id + '" class="anchor-link">§</i>');
	    }
	});
    }
};

$(function() {
    jekyllBootstrapDoc.buildSideMenu();
    jekyllBootstrapDoc.addHeadingAnchors();
});
