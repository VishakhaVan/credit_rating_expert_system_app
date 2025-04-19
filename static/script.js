function predict() {
    const name = document.getElementById("nameInput").value;

    fetch(`/predict?name=${encodeURIComponent(name)}`)
        .then(response => response.json())
        .then(data => {
            const resultDiv = document.getElementById("result");
            if (data.status === "success") {
                // First show the basic prediction result
                resultDiv.innerHTML = `
                    <p><strong>Name:</strong> ${data.name}</p>
                    <p><strong>Predicted Rating:</strong> ${data.rating}</p>
                    <p><strong>Accuracy:</strong> ${data.accuracy}</p>
                `;

                // Then fetch the explanation
                fetch(`/explanation?name=${encodeURIComponent(name)}`)
                    .then(res => res.json())
                    .then(expData => {
                        if (expData.status === "success") {
                            resultDiv.innerHTML += `
                                <p><strong>Explanation:</strong> ${expData.explanation}</p>
                            `;
                        } else {
                            resultDiv.innerHTML += `
                                <p style="color: red;"><strong>Explanation Error:</strong> ${expData.message}</p>
                            `;
                        }
                    });
            } else {
                resultDiv.innerHTML = `<p style="color: red;">${data.message}</p>`;
            }
        })
        .catch(error => {
            document.getElementById("result").innerHTML = `<p style="color: red;">Error: ${error}</p>`;
        });
}



function getAllEvaluations() {
    fetch('/evaluations')
        .then(res => res.json())
        .then(data => {
            const resultDiv = document.getElementById("result");
            if (data.status === "success") {
                resultDiv.innerHTML = "<h3>All Evaluations:</h3><ul>" +
                    data.evaluations.map(e => `<li>${e.name}: ${e.accuracy}</li>`).join('') +
                    "</ul>";
            }
        });
}

function getOverallAccuracy() {
    fetch('/accuracy')
        .then(res => res.json())
        .then(data => {
            const resultDiv = document.getElementById("result");
            if (data.status === "success") {
                resultDiv.innerHTML = `<p><strong>Overall Accuracy:</strong> ${data.accuracy.toFixed(2)}%</p>`;
            }
        });
}

function predictFromParams() {
    const income = document.getElementById("incomeInput").value;
    const debts = document.getElementById("debtsInput").value;
    const creditScore = document.getElementById("Score").value;
    const amountOwed = document.getElementById("amountOwed").value;
    const creditMix = document.getElementById("creditMix").value;
    const creditHistory = document.getElementById("creditHistory").value;
    const newCredit = document.getElementById("newCredit").value;

    const params = new URLSearchParams({
        income: income.trim(),
        debts: debts.trim(),
        creditScore: creditScore.trim(),
        amountOwed: amountOwed.trim(),
        creditMix: creditMix.trim(),
        creditHistory: creditHistory.trim(),
        newCredit: newCredit.trim()
    });
    fetch(`/predict_params?${params.toString()}`)

        // fetch(`/predict_params?income=${encodeURIComponent(income)}&debts=${encodeURIComponent(debts)}&creditScore=${encodeURIComponent(creditScore)}&amountOwed=${encodeURIComponent(amountOwed)}&creditMix=${encodeURIComponent(creditMix)}&creditHistory=${encodeURIComponent(creditHistory)}&newCredit=${encodeURIComponent(newCredit)}

        // `)
        .then(response => response.json())
        .then(data => {
            const resultDiv = document.getElementById("result");
            if (data.status === "success") {
                resultDiv.innerHTML = `
                    <p><strong>Income:</strong> ${data.income}</p>
                    <p><strong>Debts:</strong> ${data.debts}</p>
                    <p><strong>Credit score:</strong> ${data.creditScore}</p>
                    <p><strong>Amount Owed:</strong> ${data.amountOwed}</p>
                    <p><strong>Credit Mix:</strong> ${data.creditMix}</p>
                    <p><strong>Credit History:</strong> ${data.creditHistory}</p>
                    <p><strong>New Credit:</strong> ${data.newCredit}</p>
                    <p><strong>Rating:</strong> ${data.rating}</p>
                    <p><strong>Explanation:</strong> ${data.explanation}</p>
                `;
            } else {
                resultDiv.innerHTML = `<p style="color: red;">${data.message}</p>`;
            }
        })
        .catch(error => {
            document.getElementById("result").innerHTML = `<p style="color: red;">Error: ${error}</p>`;
        });
}

