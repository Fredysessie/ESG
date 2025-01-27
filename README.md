# ESG Funds and Stocks Dashboard

Bienvenue dans le **ESG Funds and Stocks Dashboard**! Cette plateforme innovante, développée avec RShiny, permet aux utilisateurs d'analyser et de gérer des titres et des fonds ESG (Environnementaux, Sociaux et de Gouvernance).

## Fonctionnalités

- **Étude de titres ou de fonds ESG** : Analyse détaillée des performances et des caractéristiques des titres ou des fonds ESG.
- **Comparaison de fonds ou de titres ESG** : Comparaison côte à côte des performances et des caractéristiques de plusieurs fonds ou titres ESG.
- **Gestion de portefeuille** : Suivi et gestion de votre portefeuille ou de vos différents portefeuilles.
- **Évaluation des performances** : Calcul des rentabilités, Value at Risk (VAR), Bêta Bull ou Bear, et autres indicateurs financiers.

## Installation

Pour installer et exécuter ce tableau de bord, suivez les étapes ci-dessous :

1. Clonez ce dépôt :
    ```bash
    git clone https://github.com/Fredysessie/ESG.git
    ```
2. Accédez au répertoire du projet :
    ```bash
    cd ESG
    ```
3. Installez les dépendances nécessaires :
    ```R
    install.packages(c("shiny", "tidyverse", "quantmod", "PerformanceAnalytics"))
    ```
4. Lancez l'application Shiny :
    ```R
    shiny::runApp()
    ```

## Utilisation

Une fois l'application lancée, vous pouvez accéder au tableau de bord via votre navigateur web. Utilisez les différentes sections pour :

- **Analyser des titres ou des fonds ESG** : Entrez le symbole du titre ou du fonds pour obtenir une analyse détaillée.
- **Comparer des fonds ou des titres ESG** : Sélectionnez plusieurs fonds ou titres pour les comparer.
- **Gérer votre portefeuille** : Ajoutez, modifiez ou supprimez des titres dans votre portefeuille et évaluez leurs performances.

## Contribuer

Les contributions sont les bienvenues! Pour contribuer :

1. Forkez ce dépôt.
2. Créez une branche pour votre fonctionnalité ou correction de bug (`git checkout -b feature/nom-de-la-fonctionnalité`).
3. Commitez vos modifications (`git commit -m 'Ajout de la fonctionnalité'`).
4. Poussez vers la branche (`git push origin feature/nom-de-la-fonctionnalité`).
5. Ouvrez une Pull Request.

## Licence

Ce projet est sous licence MIT. Voir le fichier LICENSE pour plus de détails.
