
library(dplyr)
df<-read.csv("C:/Users/user/Desktop/ECE/superheroes_nlp_dataset.csv")
# Après avoir chargé df
#print("Distribution des valeurs brutes dans 'creator':")
#print(table(df$creator))

#print("Distribution des valeurs brutes dans 'type_race':")
#print(table(df$type_race))

# Table pour la colonne 'creator' (Créateur/Éditeur)
  equivalence_creator <- data.frame(
        brut = c("DC", "DC Comix", "Marvel", "Marvel Comics, Inc."),
         standard = c("DC Comics", "DC Comics", "Marvel Comics", "Marvel Comics")
     )
  
    Table pour la colonne 'type_race' (Type de Race/Espèce)
         equivalence_type <- data.frame(
          brut = c("Mutate", "Homo Superior", "Human (Enhanced)", "God / Eternal"),
          standard = c("Mutant", "Mutant", "Humain", "Dieu / Éternel")
       )
         # 1. Nettoyage de 'creator'
    df_cleaned <- df %>%
          left_join(equivalence_creator, by = c("creator" = "brut")) %>%
          mutate(creator = ifelse(!is.na(standard), standard, creator)) %>%
         select(-standard) %>%  # Suppression de la colonne temporaire
          
          # 2. Nettoyage de 'type_race'
         left_join(equivalence_type, by = c("type_race" = "brut")) %>%
          mutate(type_race = ifelse(!is.na(standard), standard, type_race)) %>%
          select(-standard) # Suppression de la colonne temporaire
    
    # Vous devriez voir les variations brutes disparaître au profit des termes standard
    print(table(df_cleaned$creator))
    print(table(df_cleaned$type_race))
    
    #Suppression des Colonnes de Type "Texte Libre"
    df<- df_cleaned %>%
      select(-history_text, -powers_text)
    
    #Suppression des Colonnes Trop Peu Renseignées
    
    # Calcul du pourcentage de NA par colonne
    na_percentage <- colMeans(is.na(df_reduced)) * 100
    
    # Afficher les pourcentages pour identifier les colonnes à supprimer
    print(na_percentage[na_percentage > 0])
    
    # la valeur du seuil a respecter
    seuil_na <- 70
    # Identifier les colonnes à supprimer (celles au-dessus du seuil)
    cols_drop <- names(na_percentage[na_percentage >= seuil_na])
    
    # suppression des colonnes ayant 70% des elts manquants
    df <- df %>%
      select(-all_of(cols_drop))
    # opérons une réduction d'échantillons (i.e. lignes) si peu de lignes sont concernées.
    
    # Compter le nombre initial de lignes
    initial_rows <- nrow(df)
    
    # Identifier les lignes avec au moins un NA dans les colonnes restantes
    rows_with_na <- sum(!complete.cases(df))
    
    # Calculer le pourcentage de lignes à supprimer
    percentage_to_drop <- (rows_with_na / initial_rows) * 100
    
    print(paste("Nombre de lignes avec NA restantes :", rows_with_na))
    print(paste("Pourcentage de lignes à supprimer :", round(percentage_to_drop, 2), "%"))
    # Suppression des lignes
    df_final <- na.omit(df)    
    #Estimation du Nombre Total de Lignes Supprimées
    # Nombre initial de lignes du jeu de données original
    initial_total_rows <- nrow(df) # Utilisez df, le jeu de données avant toute modification
    
    # Nombre final de lignes après toutes les suppressions de l'Étape 3
    final_total_rows <- nrow(df_final)
    
    # Calcul du total supprimé
    total_deleted_rows <- initial_total_rows - final_total_rows
    total_deleted_percentage <- (total_deleted_rows / initial_total_rows) * 100
    
    print(paste("Nombre initial de lignes :", initial_total_rows))
    print(paste("Nombre total de lignes supprimées (Étape 3) :", total_deleted_rows))
    print(paste("Pourcentage total du jeu de données initial supprimé :", round(total_deleted_percentage, 2), "%"))
   #Étape 4 - Corrélations
    
    library(corrplot) # Un package très utile pour la visualisation des corrélations
    
    
    numeric_df <- df_final %>%
      select(
        ends_with("_score"),
        starts_with("has_") 
  # Les variables binaires 'has_power' sont aussi numériques (0 ou 1)
      )
    
    # Convertir toutes les colonnes en numériques 
    numeric_df[] <- lapply(numeric_df, as.numeric)
    # Calcul de la matrice de corrélation
    cor_matrix <- cor(numeric_df, use = "pairwise.complete.obs")
       
    # 1. Identifier les colonnes à variance nulle (qui ne contiennent qu'une seule valeur unique)
    zero_variance_cols <- names(which(apply(numeric_df, 2, var, na.rm = TRUE) == 0))
    
    # 2. Supprimer ces colonnes du jeu de données numérique
    numeric_df_final <- numeric_df %>%
      select(-all_of(zero_variance_cols))
    
    # 3. Recalculer la matrice de corrélation avec le jeu de données corrigé
    cor_matrix <- cor(numeric_df_final, use = "pairwise.complete.obs")
    
    # (Optionnel) Remplacer les NA/NaN restants par zéro dans la matrice pour la visualisation
    cor_matrix[is.na(cor_matrix)] <- 0
    library(corrplot)
   
    # Code optimisé pour la visualisation des corrélations :
    corrplot(cor_matrix,
             method = "square",
            # type = "full",   
             tl.cex = 0.5,      
             number.cex = 0.2,  
             diag = FALSE       
    )    
    
    