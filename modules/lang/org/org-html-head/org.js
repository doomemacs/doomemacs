
function panelDiv (col, type_name) {
    return "<div class=\"card-header\" id=\"header-" + col.id + "\">"
        + "<button class=\"btn btn-link\""
        + "data-toggle=\"collapse\""
        + "data-target=\"#" + col.id + "\""
        + "aria-expanded=\"false\""
        + "aria-controls=\"" + col.id + "\""
        + "\">Toggle " + type_name + "</button>"
        + "</div>"
}

function panelCollapse(col) {
    return "<div id=\"col-" + col.id + Math.random().toString(36).substring(7)
        + "\" class=\"collapse\"></div>";
}

$( document ).ready(function() {
    $('body').attr("data-spy", "scroll");
    $('body').attr("data-target", "#toc");


    // The content (and postamble) should be in a container.
    $('#content, #postamble').wrapAll("<div class='container'></div>");
    $('#toc, .outline-2').wrapAll("<div class='row'></div>");
    $('#toc').wrap("<div class='col-md-3'></div>");
    $('.outline-2').wrapAll("<div class='col-md-9'></div>");
    $('.col-md-9').after( $('.col-md-3') );

    // $('#footnotes').appendTo('.col-sm-9');

    $('#toc').empty();
    $('#toc').addClass('nav-list');


    var navSelector = '#toc';
    var $myNav = $(navSelector);
    Toc.init($myNav);
    $('body').scrollspy({
        target: navSelector
    });

    $('.org-src-container').addClass("card-body").wrap('<div class="card"></div>');
    $('.table').addClass("card-body").wrap('<div class="card"></div>');
    $('.card-body').each(function() {$(this).wrap(panelCollapse(this))})
    $('.org-src-container').parent().each(function() {$(this).before(panelDiv(this, 'Code'))})
    $('.table').parent().each(function() {$(this).before(panelDiv(this, 'Table'))})
});
