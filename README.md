# MSc Thesis - Volatility Forecasting & Time Series Analysis in R

This repository contains selected **R scripts and models** from my **Master's thesis in Finance**, which focused on **volatility forecasting using the HAR (Heterogeneous Autoregressive) model**. The thesis explored **16 different variations** of HAR-based models to improve volatility predictions in financial markets.

## 📂 Available Files

### **1️⃣ Thesis-Related Scripts**
- **har_rv_model.R** → Implementation of HAR-RV and its variations.
- **har_rv_j_model.R** → HAR-RV-J model (incorporating jumps).
- **lhar_rv_model.R** → LHAR-RV model (leverage effect included).

### **2️⃣ Additional Studies (Time Series & Volatility Models)**
- **volatility_models.R** → Overview of various volatility models (ARCH, GARCH, etc.). *(Not part of the thesis but included as a complementary study.)*
- **time_series_analysis.R** → Time series modeling techniques beyond HAR. *(Not part of the thesis but included to demonstrate broader knowledge.)*

### **3️⃣ Data Processing Scripts**
- **data_cleaning.R** → Preprocessing and cleaning of financial data.

## 📊 Research Overview
This study aimed to evaluate the efficiency of HAR-based models in predicting market volatility. The methodology involved:
- **Developing and testing 16 variations of HAR-Type models**, incorporating different decompositions of realized volatility.
- **Analyzing the impact of jumps, signed jumps, and leverage effects** in volatility forecasting.
- **Testing the predictive power of HAR variations** using multiple loss functions and Model Confidence Set (MCS) methodology.

### **HAR-Type Models Used in the Thesis**
The thesis explored 16 variations of the HAR model, including:
- **HAR-RV** → Standard HAR model using realized volatility.
- **HAR-RV-J** → Incorporates jumps as an explanatory variable.
- **HAR-CJ** → Separates realized volatility into continuous and jump components.
- **HAR-RSV** → Uses realized semivariance instead of realized volatility.
- **HAR-RSV-J** → Adds jumps to the realized semivariance model.
- **HAR-RV-SJ** → Uses signed jumps to differentiate between positive and negative impacts.
- **HAR-RV-SSJ (I) & (II)** → Further decomposition of signed jumps into lagged weekly and monthly effects.
- **LHAR Variants** → Each of the above models was also tested with the leverage effect (negative returns).

The results showed that **weekly volatility had the strongest predictive power**, and that **negative realized semivariance and signed jumps provided more information than their positive counterparts**. However, the Model Confidence Set (MCS) analysis indicated that **no single model significantly outperformed all others in out-of-sample forecasting**.

## ⚠ Data Disclaimer
The dataset used in this research contains proprietary financial data and cannot be shared. However, the **R scripts** in this repository can be used with any similar time series dataset. Users are encouraged to apply the models with their own data.

## 🔧 How to Use
1. Clone the repository:
   ```bash
   git clone https://github.com/gustavomfreitas/MSc-Thesis-R.git
   ```
2. Navigate to the `R_Scripts/` directory and run the `.R` scripts with your dataset.

## 📫 Contact
📩 **Email:** gustavo.provento@gmail.com  
💼 **LinkedIn:** [linkedin.com/in/gustavomfreitas](https://www.linkedin.com/in/gustavomfreitas)  
📂 **GitHub:** [github.com/gustavomfreitas](https://github.com/gustavomfreitas)


