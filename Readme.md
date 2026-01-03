# Mall Customer Segmentation Analysis

This project focuses on customer segmentation using exploratory data analysis and inferential statistics to derive data-driven marketing insights.  
Rather than relying solely on descriptive clustering, the study compares **distribution-based segmentation (V1)** with **inferential-statistics-driven analysis (V2)** to evaluate the validity of common assumptions such as income-driven spending behavior.

---

## Dataset

- **Source:** Mall Customer Dataset
- **Variables:**
  - Age
  - Annual Income
  - Spending Score
  - Gender

---

## Methodology

### V1 – Distribution-Based Segmentation
- Analysis of age, annual income, and spending score distributions
- Customers segmented into 5 groups
- A high-income & high-spending segment was labeled as the **VIP group**

**Limitation:**  
This approach implicitly assumes that higher income leads to higher spending, without statistical validation.

---

### V2 – Inferential Statistical Analysis
To validate the assumptions made in V1, inferential statistical methods were applied:

- Normality testing (e.g., Shapiro–Wilk / Kolmogorov–Smirnov where appropriate)
- Group comparisons using suitable parametric or non-parametric tests
- Gender-based comparisons
- Distribution shape, median, and group dominance analysis

This step evaluates whether descriptive segmentation results hold under statistical testing.

---

## Key Findings

- The **middle-aged customer group** has the highest annual income.
- The **young customer segment** exhibits relatively higher spending scores.
- **Annual income shows limited explanatory power** in predicting spending behavior.
- **No statistically significant differences** were found between gender and:
  - Spending Score
  - Annual Income
- The **VIP group** defined in V1 was found to be **statistically insignificant** when evaluated through V2 analysis.

---

## Interpretation

- High spending behavior is **not primarily driven by income**
- Young customers demonstrate stronger engagement independent of income level
- Descriptive segmentation alone can lead to **misleading strategic conclusions**
- Inferential statistics are essential for validating segmentation logic and assumptions

---

## Business Implications

- **Primary target segment:** Young customers  
  - Income-agnostic
  - Gender-neutral approach
- Marketing strategies should focus on **engagement rather than income growth assumptions**
- Secondary initiatives can target the **36–40 age subgroup**
- Gift-oriented strategies may be developed for middle-aged and older segments to expand reach

---

## Limitations & Future Work

- The dataset lacks behavioral and engagement-related variables
- Future analyses could incorporate:
  - Purchase frequency
  - Customer interaction data
  - Time-based spending patterns
  - Behavioral segmentation features

---

## Tools & Libraries

- R / Python
- Data visualization libraries (e.g., ggplot2 / matplotlib)
- Statistical testing libraries

---

## Conclusion

This project demonstrates the importance of validating segmentation strategies using inferential statistics.  
It highlights how commonly accepted assumptions—such as income-driven spending—may fail when subjected to rigorous statistical testing, reinforcing the need for data-driven decision-making in customer analytics.
