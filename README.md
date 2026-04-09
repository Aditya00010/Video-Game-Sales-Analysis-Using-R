# Video-Game-Sales-Analysis-Using-R
This project performs data analysis and machine learning on video game sales data using R, including visualization, KNN regression, linear regression, and decision tree models.
# 🎮 Video Game Sales Analysis & Prediction (R)

## 📌 Overview

This project analyzes video game sales data and applies machine learning techniques to predict global sales. It includes data preprocessing, visualization, and multiple predictive models.

---

## 🚀 Features

* 📊 Data cleaning and preprocessing
* 📈 Data visualization using ggplot2
* 🤖 Machine Learning models:

  * K-Nearest Neighbors (KNN)
  * Linear Regression
  * Decision Tree
* 📉 Model evaluation using MSE and MAE

---

## 🛠️ Technologies Used

* R Programming
* tidyverse
* ggplot2
* FNN (KNN)
* caret
* tree
* corrplot

---

## 📂 Project Structure

```id="5p7d2c"
├── script.R        # Main R script
├── Video_Games_Sales_as_at_22_Dec_2016.csv   # Dataset
```

---

## ⚙️ How to Run

1️⃣ Open RStudio or R environment

2️⃣ Install required libraries:

```id="u9ngvm"
install.packages(c("tidyverse","FNN","corrplot","tree","caret"))
```

3️⃣ Run the script:

```id="9l6h9z"
source("script.R")
```

---

## 📊 Dataset

The dataset contains video game sales information including platform, genre, publisher, critic scores, and global sales.

---

## 📈 Models Used

### 🔹 K-Nearest Neighbors (KNN)

* Used for regression prediction of global sales
* Optimal K value selected using MSE

### 🔹 Linear Regression

* Predicts sales based on multiple features
* Evaluated using Mean Squared Error

### 🔹 Decision Tree

* Visualizes decision-making process
* Pruned for better accuracy

---

## 📉 Evaluation Metrics

* Mean Squared Error (MSE)
* Mean Absolute Error (MAE)

---

## 🧠 Future Improvements

* Add more advanced models (Random Forest, XGBoost)
* Improve feature selection
* Build interactive dashboard

---

## 🤝 Contributing

Feel free to fork and contribute!

---

## 📄 License

This project is open-source.

---

## 👨‍💻 Author

Aditya Yadav
