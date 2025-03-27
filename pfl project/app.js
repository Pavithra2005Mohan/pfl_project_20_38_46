async function analyzeText() {
    const text = document.getElementById("inputText").value;

    if (!text.trim()) {
        alert("Please enter some text.");
        return;
    }

    // Send text to Haskell backend using fetch (via HTTP)
    const response = await fetch('http://localhost:3000/analyze', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({ text: text })
    });

    if (!response.ok) {
        document.getElementById("results").innerHTML = "⚠️ Error during analysis.";
        return;
    }

    const result = await response.text();
    document.getElementById("results").innerText = result;
}
