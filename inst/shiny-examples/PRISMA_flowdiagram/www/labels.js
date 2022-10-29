/** 
 *	@param {HTMLElement} node the node to label 
 *  @param {string} label the label for the node
*/
function renderLabel(node, label) {
	var theText = node.querySelector("text");
	var attrX = theText.getAttribute("x");
	var attrY = theText.getAttribute("y");
	theText.setAttribute("y",parseFloat(attrX))
	theText.setAttribute("x",parseFloat(attrY)*-1)
	theText.setAttribute("style","transform: rotate(-90deg);")
	theText.setAttribute("dominant-baseline", "middle")
	theText.innerHTML = label;
}
// https://stackoverflow.com/questions/38881301/observe-mutations-on-a-target-node-that-doesnt-exist-yet/38882022
/** 
 *	@param {string} id the ID of the node to wait for
 *  @param {HTMLElement} parent the parent of the node
 *  @param {boolean} recursive whether or not to recurse
 *  @param {function(HTMLElement, string)} done the callback function to execute when the node appears
*/
function waitForAddedNode(id, parent, recursive, done) {
	new MutationObserver((mutations, observer) => {
		var el = document.getElementById(id)
		if (el) {
			observer.disconnect()
			done(el)
		}
	}).observe(parent || document, {
		subtree: !!recursive || !parent,
		childList: true
	})
}

/** 
 *  @param {Map <string,string>} nodeMap a map of nodes and their labels
 *  @return {MutationObserver} a mutation observer to label the node with the label whenever it changes
*/
function createLabels(nodeMap) {
	for (const [node, label] of nodeMap) {
		waitForAddedNode(node, document.getElementById("plot1"), true, (node) => {
			renderLabel(node, label)
		})
	}
}