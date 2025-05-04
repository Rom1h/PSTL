# PSTL

Ce projet étant un projet stack vous aurez donc uniquement besoin de stack 

# Aborescence de fichier 

Dans le dossier test, vous pourrez voir nos différents fichiers Haskell pour les tests, tandis que pour le code principal, vous n’aurez besoin de consulter que le dossier src.Vous trouverez également le dossier leanFile, qui contient les fichiers générés suite à notre compilation, ainsi que le dossier xmlFile, qui contient les fichiers passés en argument à notre code (fichiers générés par rondin).Enfin, dans le dossier xmlFile, vous pourrez aussi trouver les fichiers servant de tests unitaires pour nos tests fonctionnels, dans le sous-dossier test.

# Commande et utilisation 

Pour exécuter le code, il vous suffit de construire le projet avec la commande suivante :

```
stack build 
```

Ensuite, vous pourrez lancer le code avec la commande suivante :

```
stack exec PSTL-exe <nom_du_fichier_context> <nom_du_fichier_machine> 
```

Exemple d'une commande avec les noms de fichier

```
stack exec PSTL-exe ./xmlFile/Machine/testContext.xml ./xmlFile/Machine/m0.xml
```

Après avoir exécuté ces commandes, vous pourrez consulter les résultats dans le dossier LeanFile, où se trouvent notre contexte, notre machine, ainsi que le programme complet généré.

# Test 

Pour lancer les tests, il vous suffit d’exécuter la commande suivante :

```
stack test 
```
