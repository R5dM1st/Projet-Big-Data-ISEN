# Analyse et Prédiction sur les Données de Navires

Ce projet de Big Data, réalisé dans un cadre académique d'années 3 à l'ISEN, propose un pipeline d’analyse de données, de visualisation et de prédiction sur un jeu de données de navires.  
Il inclut des étapes de nettoyage, d’exploration statistique, de visualisations avancées, et de modélisation prédictive (classification et régression).

## Table des matières

- [Description](#description)
- [Installation](#installation)
- [Utilisation](#utilisation)
- [Fonctionnalités](#fonctionnalités)
- [Structure du projet](#structure-du-projet)
- [Auteurs](#auteurs)

---

## Description

Ce projet vise à :
- Nettoyer et préparer des données de navires issues d’un CSV.
- Réaliser des analyses statistiques descriptives et des visualisations (corrélation, mosaicplots).
- Construire des modèles prédictifs pour la classification (`VesselType`) et la régression (prédiction de la vitesse, SOG).
- Mesurer et illustrer la performance des modèles (matrice de confusion).

---

## Installation

1. **Cloner le dépôt**  
   ```bash
   git clone <url-du-repo>
   cd <nom-du-repo>
   ```

2. **Installer les packages R nécessaires**  
   Dans R ou RStudio :
   ```r
   install.packages(c(
     "dplyr", "nnet", "corrplot", "ggplot2", "reshape2"
   ))
   ```

3. **Préparer vos fichiers de données**  
   Placez votre fichier CSV principal (`vessel-total-clean.csv` ou `vessel-clean-final.csv`) à la racine du projet.

---

## Utilisation

1. **Nettoyage et filtrage des données**  
   Lancez `fonctionalite1.R` pour obtenir un jeu de données propre (`vessel-clean-final.csv`).

2. **Exploration et visualisations**  
   - `fonctionalite2.R` : Visualisation des types de navires et des ports les plus fréquentés.
   - `fonctionalite3.R` : Visualisation des trajectoires maritimes dans le Golfe du Mexique. ⚠️(ne pas oublier de placer le MMSI du bateau dans l'input dans l'invite de commande pour la partie de visualisation du trajet d'un bateau).
   - `fonctionalite4.R` : Analyse statistique, matrices de corrélation, mosaicplots.

3. **Modélisation prédictive**  
   - `fonctionalite5.R` : Modélisation (classification, régression), prédiction du type de navire, calcul et affichage de la matrice de confusion.

4. **Résultats**  
   Les figures et résultats sont enregistrés dans le dossier `figures/`.

---

## Fonctionnalités

- **Nettoyage automatisé** des données (remplacement des valeurs aberrantes, gestion des NA, suppression des doublons).
- **Visualisations avancées** : matrices de corrélation (`corrplot`), mosaicplots, matrice de confusion colorée.
- **Analyse descriptive** : statistiques par type de navire, exploration des valeurs manquantes.
- **Prédiction** : classification (`VesselType`), régression de la vitesse (`SOG`), calcul d’erreurs.
- **Export** des jeux de données nettoyés et des graphes.

---

## Structure du projet

```
├── README.md
├── vessel-total-clean.csv
├── vessel-clean-final.csv            # Données nettoyées (générées par script)
├── fonctionalite1.R                  # Script de nettoyage et filtrage
├── fonctionalite2.R                  # Visualisation des types de navires et ports les plus fréquentés
├── fonctionalite3.R                  # Visualisation des trajectoires maritimes dans le Golfe du Mexique
├── fonctionalite4.R                  # Analyse, visualisations, corrélation, mosaicplot
├── fonctionalite5.R                  # Modélisation, prédiction, matrice de confusion
├── figures/
│   ├── matrice_correlation_quantitatives.png
│   ├── mosaicplot_VesselType_vs_Status.png
│   ├── mosaicplot_VesselType_vs_Cargo.png
│   └── ...
└── ...
```

---

## Auteurs

- [Emile Duplais](https://github.com/R5dM1st)
- [Matteo D'ettore](https://github.com/matteodettore)
- [Alex LETOUZE](https://github.com/Alex-LTZ)

---

## Licence

Projet académique BIG DATA ISEN A3 : 13/06/2025
