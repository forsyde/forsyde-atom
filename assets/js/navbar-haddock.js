var jekyllBootstrapDoc = {
    buildSideMenu: function() {
	var html = '';
	// console.log("HALLO!!");
	    
	$('#interface').each(function() {
	    var h123 = $(this).find('a[href]:not([data-no-menu])');

	    var sections = [];
	    h123.each(function(ix, elem) {
		if(elem.firstChild.nodeName == "H1")
		    sections.push([$(this),[]]);
		else
		    if (sections.length && (elem.firstChild.nodeName == "H2" || elem.firstChild.nodeName == "H3"))
			sections[sections.length - 1][1].push($(this));
	    })	    
	    // console.log("Inside!!");
	    // console.log("init: %o", h123);
	    // console.log("split: %o", sections);

	    sections.forEach(function(section) {
	    	console.log("section: %o", section[0][0]);
	    	console.log("content: %o", section[1]);

		html += '<li><a href="' + section[0][0].getAttribute("href") + '">'
		        + section[0].clone().children().end().text() + '</a>';

		if (section[1].length){
		    html += '<ul class="nav">';
	    	    section[1].forEach(function(subsection) {
	    		console.log("subsect: %o", subsection);
	    		html += '<li><a href="' + subsection[0].getAttribute("href")
			        + '">' + subsection.clone().children().end().text() + '</a></li>';	
	    	    });
	    	    html += '</ul>';
		}
		html += '</li>';
	    })
	});

	var title = $('#module-header > p.caption')[0].innerText;
	console.log("TITLE: %o", title);

	html = '<li style="padding-top: 10px"><strong><a style="color:#000" href="#package-header">'
	    + title
	    + '</a></strong></li>' + html;


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
