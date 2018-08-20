scroller_holder = 0;
$(document).on('shiny:inputchanged', function(event) {
  //console.log(event.name);
  if (event.name === "sidebarCollapsed" && scroller_holder == 0){
    //console.log('triggered');
    scroller_holder = scroller_holder +1;
    $("#todayBox").click(function() {
      $("#coordinate").animate({
          scrollTop: $("#today").offset().top
        }, 1000);
      });
        $("#newBox").click(function() {
      $("#coordinate").animate({
          scrollTop: $("#newPatient").offset().top
        }, 1000);
      });
      $("#pastBox").click(function() {
        $("#coordinate").animate({
            scrollTop: $("#pastDue").offset().top
        }, 1000);
      });
      $("#scheduleDischarge").click(function() {
        $("#coordinate").animate({
            scrollTop: $("#estimated_discharge").offset().top
        }, 1000);
      });
      $("#expiredBox").click(function() {
        $("#coordinate").animate({
          scrollTop: $("#expired").offset().top
        }, 1000);
      });
      $("#crop_ref_box").click(function() {
        $("#master_coordinate").animate({
          scrollTop: $("#crop_master").offset().top
        }, 1000);
      });
      $("#cadh_ref_box").click(function() {
        $("#master_coordinate").animate({
          scrollTop: $("#cadh_master").offset().top
        }, 1000);
      });
      $("#adh_ref_box").click(function() {
        $("#master_coordinate").animate({
          scrollTop: $("#adh_master").offset().top
        }, 1000);
      });
      $("#retreat_ref_box").click(function() {
        $("#master_coordinate").animate({
          scrollTop: $("#retreat_master").offset().top
        }, 1000);
      });
  }});