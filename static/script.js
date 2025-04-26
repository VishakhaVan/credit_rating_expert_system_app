// Functions to handle dropdown selection changes
function updateIncome() {
    const select = document.getElementById("incomeSelect");
    const input = document.getElementById("incomeInput");
    if (select.value) {
        input.value = select.value;
    }
}

function updateDebts() {
    const select = document.getElementById("debtsSelect");
    const input = document.getElementById("debtsInput");
    if (select.value) {
        input.value = select.value;
    }
}

function updateScore() {
    const select = document.getElementById("scoreSelect");
    const input = document.getElementById("Score");
    if (select.value) {
        input.value = select.value;
    }
}

function updateAmount() {
    const select = document.getElementById("amountSelect");
    const input = document.getElementById("amountOwed");
    if (select.value) {
        input.value = select.value;
    }
}

function updateMix() {
    const select = document.getElementById("mixSelect");
    const input = document.getElementById("creditMix");
    if (select.value) {
        input.value = select.value;
    }
}

function updateHistory() {
    const select = document.getElementById("historySelect");
    const input = document.getElementById("creditHistory");
    if (select.value) {
        input.value = select.value;
    }
}

function updateNewCredit() {
    const select = document.getElementById("newCreditSelect");
    const input = document.getElementById("newCredit");
    if (select.value) {
        input.value = select.value;
    }
}

function predict() {
    const name = document.getElementById("nameInput").value;

    if (!name.trim()) {
        document.getElementById("result").innerHTML = `<p style="color: red;">Please enter a customer name</p>`;
        return;
    }

    document.getElementById("result").innerHTML = '<p>Loading results...</p>';
    document.getElementById("risk-category").innerHTML = '';

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
                    <p><strong>Tips:</strong> ${data.tips}</p>
                `;

                // Update risk category
                updateRiskCategory(data.rating);

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
    document.getElementById("result").innerHTML = '<p>Loading evaluations...</p>';
    document.getElementById("risk-category").innerHTML = '';

    fetch('/evaluations')
        .then(res => res.json())
        .then(data => {
            const resultDiv = document.getElementById("result");
            if (data.status === "success") {
                if (data.evaluations.length === 0) {
                    resultDiv.innerHTML = "<p>No evaluations available yet.</p>";
                } else {
                    resultDiv.innerHTML = "<h3>All Evaluations</h3><div class='evaluation-list'>";
                    data.evaluations.forEach(e => {
                        resultDiv.innerHTML += `<div class="evaluation-item">
                            <span class="customer-name">${e.name}</span>
                            <span class="accuracy-value">Accuracy: ${e.accuracy}</span>
                        </div>`;
                    });
                    resultDiv.innerHTML += "</div>";
                }
            } else {
                resultDiv.innerHTML = `<p style="color: red;">${data.message || "Failed to load evaluations"}</p>`;
            }
        })
        .catch(error => {
            document.getElementById("result").innerHTML = `<p style="color: red;">Error loading evaluations: ${error}</p>`;
        });
}

function getOverallAccuracy() {
    document.getElementById("result").innerHTML = '<p>Calculating accuracy...</p>';
    document.getElementById("risk-category").innerHTML = '';

    fetch('/accuracy')
        .then(res => res.json())
        .then(data => {
            const resultDiv = document.getElementById("result");
            if (data.status === "success") {
                resultDiv.innerHTML = `
                    <div class="accuracy-container">
                        <h3>System Performance</h3>
                        <p class="accuracy-result"><strong>Overall Accuracy:</strong> ${data.accuracy.toFixed(2)}%</p>
                        <p>Based on all historical predictions and actual outcomes</p>
                    </div>
                `;
            } else {
                resultDiv.innerHTML = `<p style="color: red;">${data.message || "Failed to calculate accuracy"}</p>`;
            }
        })
        .catch(error => {
            document.getElementById("result").innerHTML = `<p style="color: red;">Error calculating accuracy: ${error}</p>`;
        });
}

function validateInputs() {
    const fields = [
        { id: "incomeInput", name: "Income" },
        { id: "debtsInput", name: "Number of Debts" },
        { id: "Score", name: "Credit Score" },
        { id: "amountOwed", name: "Amount Owed" },
        { id: "creditMix", name: "Credit Mix" },
        { id: "creditHistory", name: "Credit History" },
        { id: "newCredit", name: "New Credit" }
    ];
    
    let missingFields = [];
    
    fields.forEach(field => {
        const value = document.getElementById(field.id).value.trim();
        if (!value) {
            missingFields.push(field.name);
        }
    });
    
    if (missingFields.length > 0) {
        document.getElementById("result").innerHTML = `
            <p style="color: red;">Please fill in the following fields: ${missingFields.join(", ")}</p>
        `;
        return false;
    }
    
    return true;
}

function updateRiskCategory(rating) {
    const riskDiv = document.getElementById("risk-category");
    
    // Remove any existing classes
    riskDiv.classList.remove("risk-low", "risk-medium", "risk-high");
    
    if (rating === "excellent") {
        riskDiv.innerHTML = "Low Risk";
        riskDiv.classList.add("risk-low");
    } else if (rating === "good" || rating === "fair") {
        riskDiv.innerHTML = "Medium Risk";
        riskDiv.classList.add("risk-medium");
    } else if (rating === "poor") {
        riskDiv.innerHTML = "High Risk";
        riskDiv.classList.add("risk-high");
    } else {
        riskDiv.innerHTML = "";
    }
}

function predictFromParams() {
    if (!validateInputs()) {
        return;
    }

    document.getElementById("result").innerHTML = '<p>Analyzing credit profile...</p>';
    document.getElementById("risk-category").innerHTML = '';

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
        .then(response => response.json())
        .then(data => {
            const resultDiv = document.getElementById("result");
            if (data.status === "success") {
                resultDiv.innerHTML = `
                    <div class="result-header">Credit Analysis Results</div>
                    <p><strong>Income:</strong> $${Number(data.income).toLocaleString()}</p>
                    <p><strong>Debts:</strong> ${data.debts}</p>
                    <p><strong>Credit Score:</strong> ${data.creditScore}</p>
                    <p><strong>Amount Owed:</strong> $${Number(data.amountOwed).toLocaleString()}</p>
                    <p><strong>Credit Mix:</strong> ${data.creditMix}</p>
                    <p><strong>Credit History:</strong> ${data.creditHistory} years</p>
                    <p><strong>New Credit:</strong> ${data.newCredit}</p>
                    <p><strong>Rating:</strong> ${data.rating.charAt(0).toUpperCase() + data.rating.slice(1)}</p>
                    <div class="explanation-section">
                        <p><strong>Explanation:</strong> ${data.explanation}</p>
                    </div>
                    <div class="tips-section">
                        <p><strong>Tips for Improvement:</strong> ${data.improvement_tips}</p>
                    </div>
                `;

                // Update risk category
                updateRiskCategory(data.rating);
            } else {
                resultDiv.innerHTML = `<p style="color: red;">${data.message}</p>`;
            }
        })
        .catch(error => {
            document.getElementById("result").innerHTML = `<p style="color: red;">Error: ${error}</p>`;
        });
}

// Add additional styling to the results section
document.addEventListener('DOMContentLoaded', function() {
    // Additional initialization if needed
});