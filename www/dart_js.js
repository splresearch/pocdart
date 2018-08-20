function copyFormatter_all(data){
  if (typeof(dailyInput) == "undefined"){
    dailyInput = 0;
  }
  
  if (typeof(reportInput) == "undefined"){
    reportInput = 0;
  }
  
  var Outcomes = $('#TableCaption').html().split('<br>');
  Outcomes.shift();
  var message = Math.random();
  Shiny.onInputChange("copyMerger_all",message);
  return Outcomes.join('\n') + '\n\n'+ data.replace(/-  - /g, "");
}

function toggleModal() {
  if (modalLauncher == 0){
    modalLauncher = modalLauncher + 1;
  } else {
    modalLauncher = 0;
    $('#shiny-modal').modal('toggle');
  }
}

function toggleParent() {
  $('#shiny-modal').modal('toggle');
}

function openPSU() {
    var link = "http://pocdart.org/redcap/surveys/?s=CR7A997E7Y";
    window.open(link,'_parent');
}