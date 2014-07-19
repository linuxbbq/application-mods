var elementNames = new Array("object","applet");

for(var i = 0; i < elementNames.length; i++) {
    elements = document.getElementsByTagName(elementNames[i]);
    for(var j = 0; j < elements.length; j++) {
        var button = document.createElement("button");
        button.appendChild(document.createTextNode("<" + elementNames[i] + ">"));
        elements[j].parentNode.insertBefore(button, elements[j]);
        button.onclick = function() {
            this.nextSibling.style.display="";
            this.style.display="None";
            return false;
        }
        elements[j].style.display="None";
    }
}
