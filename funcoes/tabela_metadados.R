tabela_metadados <- function(tabela_de_entrada,sufixo_da_tabela_saida,vector_vars_drop){

    mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
    }
   

    # Índice e Nome das colunas
    metadata_table <- data.frame(index=1:length(colnames(tabela_de_entrada)),columns=colnames(tabela_de_entrada))

    # Tipo do dado de cada coluna
    metadata_table['class'] <- sapply(tabela_de_entrada,function(x) paste(class(x), collapse = "; "))

    # Número de valores únicos (Cardinalidade)
    metadata_table['nunique'] <- sapply(tabela_de_entrada,function(x) length(unique(na.omit(x))))

    # Número de valores únicos (Cardinalidade) (+ 1 se tiver valor nulo)
    metadata_table['nunique_with_na'] = sapply(tabela_de_entrada,function(x) length(unique(x)))

    # Número de valores nulos e Porcentagem de valores nulos
    metadata_table['na_value_count'] <- sapply(tabela_de_entrada,function(x) sum(is.na(x)))
    metadata_table['p_na_value_count'] <- metadata_table['na_value_count']/dim(tabela_de_entrada)[1]

    # Porcentagem de valores repetidos da moda em relação a todos os valores
    metadata_table['p_mode_value_natoo'] <- sapply(tabela_de_entrada,function(x) sum(match(x,mode(x)),na.rm=TRUE))/dim(tabela_de_entrada)[1]
    
    # Porcentagem de valores repetidos da moda em relação aos valores não nulos
    metadata_table['p_mode_value_notna'] <- sapply(tabela_de_entrada,function(x) sum(match(x,mode(na.omit(x))),na.rm=TRUE))/sapply(tabela_de_entrada,function(x) length(na.omit(x)))

    # Adiciona variáveis que devem ser ignoradas no futuro 
    metadata_table['drop'] <- colnames(tabela_de_entrada) %in% vector_vars_drop

    assign(x=paste0('metadata_table_',sufixo_da_tabela_saida),value=metadata_table,envir=.GlobalEnv)
    return( list(paste('Nome da tabela de entrada:',deparse(substitute(tabela_de_entrada))),
            paste('Dim da tabela de entrada:',paste0('(',paste(dim(tabela_de_entrada),collapse=","),')')), 
            paste('Atributos a serem ignorados no futuro:',deparse(vector_vars_drop)),
            paste0('Nome da tabela abaixo: metadata_table_',sufixo_da_tabela_saida), 
            metadata_table ) )
}
