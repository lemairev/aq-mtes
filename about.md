## Introduction

**Cette application a été créée dans le cadre du [concours](https://www.lcsqa.org/sites/default/files/Actualit%C3%A9s/A6-2018-09-REGLEMENT%20CONCOURS%20QA%20v03092018.pdf) sur la valorisation des données relatives à la qualité de l'air extérieur proposé par le Ministère de l'environnement et de la transition écologique et solidaire.**

Elle permet simplement de **s'informer sur la qualité de l'air près de chez soi** en s'appuyant sur des mesures ([AASQA](https://atmo-france.org/)) et des simulations numériques ([Prev'Air](http://www2.prevair.org/)). Elle offre aussi la possibilité aux utilisateurs **d'explorer de façon ludique les différentes sources de pollution dans leur commune** (ou département...) en s'appuyant sur l['Inventaire National Spatialisé](http://emissions-air.developpement-durable.gouv.fr/) et **d'explorer les différents épisodes de pollution** (répertoriés par le [Laboratoire Central de Surveillance de la Qualité de l'Air](https://www.lcsqa.org/fr)) au sein de leur département ou au niveau national (métropolitain). Enfin un dernier onglet se penche sur les tendances de recherche google ainsi que sur des tweets collectés pendant un mois afin **d'estimer l'intérêt social pour la qualité de l'air.**

Les chapitres suivants détailleront **les sources de données** et fourniront des liens **pour aller plus loin** dans la compréhension des polluants et des sources de pollution atmosphérique.

## Découverte de la qualité de l'air près de chez soi

La [pollution atmosphérique](http://www2.prevair.org/content/qualite-de-lair-et-enjeux-atmospheriques) se définit comme la surabondance de gaz et particules nocives
dans l'atmosphère. Cette surabondance est défini par des valeurs [seuils](http://www2.prevair.org/content/normes-de-qualit%C3%A9-de-lair-et-gestion-de-la-pollution) qui sont fixées par le droit Européen. La pollution de l'air a des [effets](https://www.ecologique-solidaire.gouv.fr/pollution-lair-origines-situation-et-impacts#e0) néfastes sur la santé humaine et l'environnement (impact sur le climat, l'agriculture...). Elle est influencée par plusieurs facteurs: 

- les [émissions](http://www2.prevair.org/content/origine-et-sources-de-pollution) de polluants primaires et des précurseurs de polluants secondaires (i.e. formés à partir d'autres composés)
- les conditions météorologiques (condition de stagnation qui favorise l'accumulation de polluants)
- le transport longue distance des polluants (d'un pays à l'autre par exemple)

Pour toutes ces différentes raisons, il est important de bien déterminer et surveiller la qualité de l'air en France. Cette mission est confiée à un ensemble d'[institutions](https://www.lcsqa.org/fr/les-acteurs) qui fournissent les données brutes exploitées dans cette application. 

## Description des données exploitées et sources

### Prévision de la qualité de l'air: [Prev'Air](http://www2.prevair.org/)

Prev'Air propose un ensemble de prévisions de qualité de l’air pour différents polluants (ozone (O<sub>3</sub>), particules (PM<sub>2.5</sub> & PM<sub>10</sub>) et oxydes d'azote (NO<sub>2</sub>)) allant jusqu'à J+3. Les données sont accessibles chaque jour à 9h et couvrent la France métropolitaine avec une résolution spatiale de l'ordre de 2km. On distingue deux types de données:

- les prévisions (J-J+3): simulations numériques et correction statistique
- l'analyse (J-1): représentation optimale basée sur la modélisation et les observations des concentrations de surface (en µg/m3) des principaux polluants à savoir ozone (O3) et particules (PM10). Les valeurs fournies sont relatives soit au maximum horaire de la journée soit à la moyenne journalière.

Pour plus d'information sur la chaîne de simulation mise en oeuvre, veuillez suivre ce [lien](http://www2.prevair.org/content/propos-de-prevair) et pour télécharger les données [celui-ci](https://www.data.gouv.fr/fr/datasets/mise-a-disposition-de-donnees-de-qualite-de-lair-sur-la-france-www-prevair-org-1/#_).


### Mesures des concentrations aux stations: [AASQA](https://atmo-france.org/category/aasqa/) 

En France, la surveillance de la qualité de l’air extérieur est confiée aux 18 Associations Agréées de Surveillance de la Qualité de l’Air (AASQA) membres de la fédération [ATMO](https://atmo-france.org/). Elles entretiennent et gèrent les stations de mesure qui participent à cette surveillance. Elles réalisent aussi d'autres missions avec par exemple des simulations à l'échelle de la région ou la constitution d'inventaire d'émissions régionaux. 

Les données mesurées sont transmises au LCSQA qui les intègre dans la base nationale de données de la qualité de l'air (GEOD'AIR) et les transmet au niveau européen à l'[agence environnementale européenne](https://www.eea.europa.eu/). Le LCSQA est mandaté par le Ministère en charge de l’Environnement pour réaliser le rapportage réglementaire de la qualité de l’air selon les termes des Directives et Décisions européennes. 

Les données disponibles concernent les concentrations horaires de plusieurs polluants :

- Ozone (O<sub>3</sub>)
- Dioxyde d’azote (NO<sub>2</sub>)
- Dioxyde de soufre (SO<sub>2</sub>)
- Particules de diamètre inférieur à 10 µm (PM<sub>10</sub>)
- Particules de diamètre inférieur à 2,5 µm (PM<sub>2.5</sub>)
- Monoxyde de carbone (CO)

Dans le cadre de cette application, on n'exploite pas les données de monoxyde de carbone et de dioxyde de soufre. Dans un souci d'harmonisation, on s'est limité aux données prédites par Prev'Air. Les observations sont disponibles [ici](https://www.data.gouv.fr/fr/datasets/donnees-temps-reel-de-mesure-des-concentrations-de-polluants-atmospheriques-reglementes-1/) ou [ici](http://discomap.eea.europa.eu/) si l'on souhaite récupérer les données de plusieurs pays européens. 

### Emissions de polluants au niveau communal: [INS](http://emissions-air.developpement-durable.gouv.fr/)

Comme signalé dans l'introduction, les émissions de polluants jouent un rôle prépondérant dans la qualité de l'air. L'application permet de découvrir au niveau de sa commune (grâce à l'INS) pour l'année 2007 les sources de pollution principales. Elle permet aussi d'aggréger les données du département à la France entière. 

On se limite ici à une dizaine d'espèces allant des particules aux oxydes d'azote. Le méthane est aussi pris en compte, en en sa qualité de précurseur d'ozone. Pour de plus amples informations sur les espèces vous pouvez visiter le site du [CITEPA](https://www.citepa.org/fr/air-et-climat/polluants/) qui est chargé de la réalisation de l'inventaire national des polluants atmosphériques.  

### Base de données des épisodes de pollution: [LCSQA](https://www.lcsqa.org/fr)

L'application permet aussi d'explorer les épisodes de pollution recensés depuis avril 2015 par le LCSQA dans le cadre de sa mission de [vigilance atmosphérique](https://www.lcsqa.org/fr/vigilance-atmospherique). Ces données sont disponibles [ici](
https://www.lcsqa.org/fr/vigilance-atmospherique/episodes/liste) et répertorient la date, la région, le département ainsi que le polluant et le seuil dépassé.

### Autres: [Twitter](https://twitter.com/) & [Google Trends](https://trends.google.fr/trends/?geo=FR)

Les données ont été collectées sur [Twitter](https://twitter.com/) pendant un mois toutes les 12h à l'aide du package R, [rtweet](https://rtweet.info/). Deux types de tweets ont été recueillis:

- Qualité de l'air: requête de mots-clefs tels que #pollutiondelair ou #qualitedelair
- Changement climatique: requête de mots-clefs tels que #changementclimatique et #rechauffementclimatique

Diverses requêtes ont été réalisées à l'aide de [Google Trends](https://trends.google.fr/trends/?geo=FR) afin d'évaluer les recherches portant sur la qualité de l'air, le changement climatique et de les mettre en perspective avec les épisodes de pollution en France. 



