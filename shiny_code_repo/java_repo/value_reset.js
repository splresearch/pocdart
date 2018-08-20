Shiny.addCustomMessageHandler('resetValue', function(variableName) {
  console.log("TEST");
  Shiny.onInputChange(variableName, 'D');
});