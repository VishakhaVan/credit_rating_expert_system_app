
function predict() {
    const name = document.getElementById("nameInput").value;

    fetch(`/predict?name=${encodeURIComponent(name)}`)
        .then(response => response.json())
        .then(data => {
            const resultDiv = document.getElementById("result");
            if (data.status === "success") {
                resultDiv.innerHTML = `
                    <p><strong>Name:</strong> ${data.name}</p>
                    <p><strong>Predicted Rating:</strong> ${data.rating}</p>
                    <p><strong>Accuracy:</strong> ${data.accuracy}</p>
                `;
            } else {
                resultDiv.innerHTML = `<p style="color: red;">${data.message}</p>`;
            }
        })
        .catch(error => {
            document.getElementById("result").innerHTML = `<p style="color: red;">Error: ${error}</p>`;
        });
}
