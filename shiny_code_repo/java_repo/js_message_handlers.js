modalLauncher = 0;
// These functions are called by the handlers, which passes the message
function piwikUpdate(message){
  // Push user ID to PIWIK for tracking
  //_paq.push(['setUserId', message]);
}

function piwikEMR(message){
  //_paq.push(['trackEvent', 'Button',
  //'Pressed', 'Confirmed Copy', 'Holder']);
}

function statUpdater(message){
  arrayInputs = message;
  idInput = message[0];
  dailyInput = message[2];
  reportInput = message[3];
  tabInput = message[1];
}

function closeModal(message){
  document.getElementById("dc-survey").onload = function() {toggleModal()};
}

function resetShiny(message){
  //_paq.push(['trackEvent', 'Interaction',
  //'Button', 'Manual Reset', 'Holder']);
  history.go(0);
}

function disableButton(message){
  var ID = "#" + message;
  console.log(ID);
  $(ID).attr('disabled','disabled');
}

function piwikLinker(message){
  if (message === 'Admission'){
      _paq.push(['trackEvent', 'Interaction',
      'Hyperlink', 'Admission', 'Holder']);
  } else {
      _paq.push(['trackEvent', 'Interaction',
      'Hyperlink', 'Survey', 'Holder']);
  } 
}

function piwikTraining(){
      _paq.push(['trackEvent', 'Interaction',
      'Hyperlink', 'Reference Material', 'Holder']);
}

Shiny.addCustomMessageHandler("handler1", piwikUpdate );
Shiny.addCustomMessageHandler("emrButton", piwikEMR );
Shiny.addCustomMessageHandler("statUpdater", statUpdater);
Shiny.addCustomMessageHandler("closeModal", closeModal);
Shiny.addCustomMessageHandler("resetShiny", resetShiny);
Shiny.addCustomMessageHandler("disableButton", disableButton);