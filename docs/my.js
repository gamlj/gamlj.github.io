<script>
$(document).ready(function () {

    if ($(".root")[0]){
      return
    };
    $('.navbar-toggle').show();
    $('.navbar').css('display', 'none');
    $('#sidebar > h2 ').append("<a id='navbut' href=''><i class='fa fa-bars'></i></a>");
    $('#navbut').on( "click", function(e) {
               $('.navbar').toggle();
               e.preventDefault();
       } );
      $('.navbar-toggle').on('click', function(e) {
               $('.navbar').toggle();
               e.preventDefault();
      });       
    $(window).scroll(function (event) {
        $('.navbar').css('display', 'none');
    });       
});
</script>