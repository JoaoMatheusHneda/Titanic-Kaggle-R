
#------------------------------------------------------------------------------------------------------
# Variável Qualitativa
calcula_tab_frequencia <- function(tabela_de_entrada,nome_variavel,decreasing = NA){
    tabela_freq <- data.frame(table(tabela_de_entrada[nome_variavel],useNA = "ifany"))
    names(tabela_freq) <- c('Category','Frequency')
    tabela_freq['Percentage'] <- prop.table(table(tabela_de_entrada[nome_variavel],useNA = "ifany"))*100
    if(decreasing %in% c(TRUE,FALSE) ){
        tabela_freq <- tabela_freq[order(tabela_freq[['Frequency']],decreasing=decreasing),]
    }
    tabela_freq['Cumulative.Frequency'] <- cumsum(tabela_freq['Frequency'])
    tabela_freq['Cumulative.Percentage'] <- cumsum(tabela_freq['Percentage'])
    tabela_freq['Percentage'] <- round(tabela_freq['Percentage'],2)
    tabela_freq['Cumulative.Percentage'] <- round(tabela_freq['Cumulative.Percentage'],2)
    
    
    
    assign(x=paste0('table_count_prop',nome_variavel),value=tabela_freq,envir=.GlobalEnv)

    return( 
        list(
            paste0('Nome da tabela abaixo: table_count_prop',nome_variavel), 
            tabela_freq 
            )
        )
}

## Frequency and Percentage
grafico_tab_frequencia <- function(table_count_prop,nome_variavel,cor_grafico,hjust_e=-0.12,hjust_d=0.12,size=6,tx1=0.2,tx2=1.05){
    gg_count_prop <- ggplot2::ggplot(aes(x=Category,y=Frequency),data=table_count_prop) + ggplot2::theme_classic() +
                                                        ggplot2::geom_col(fill=cor_grafico) + 
                                                        ggplot2::geom_text(aes(label=Frequency,
                                                                                #x=Category-hjust_e,
                                                                                y=Frequency+tx1*mean(Frequency)),
                                                                #nudge_x = hjust_e,#hjust=hjust_e,
                                                                #position = position_dodge(0.9),
                                                                position = position_nudge(x=hjust_e,y=0.9),
                                                                size=size) +
                                                        ggplot2::geom_text(aes(label=paste0('(',Percentage,'%)'),
                                                                                #x=Category+hjust_d,
                                                                                y=Frequency+tx1*mean(Frequency)),
                                                                #nudge_x = hjust_d,#hjust=hjust_d,
                                                                #position = position_dodge(0.9),
                                                                position = position_nudge(x=hjust_d,y=0.9),
                                                                size=size) +
                                                        expand_limits(y=max(table_count_prop['Frequency'])*tx2) +
                                                        xlab(nome_variavel) + 
                                                        ylab("Frequência")
     assign(x=paste0('gg_count_prop',nome_variavel),value=gg_count_prop,envir=.GlobalEnv)
    return( 
        list(paste0('Nome do gráfico: gg_count_prop',nome_variavel)#, 
            #gg_count_prop 
            )
        )
        
}


## Only Percentage with coord_flip()
grafico_tab_frequencia2 <- function(table_count_prop,nome_variavel,cor_grafico,size,tx1=0.06,tx2=1.07,coord_flip=TRUE){
    gg_count_prop <-  ggplot2::ggplot(aes(x=Category,y=Frequency),data=table_count_prop) +  ggplot2::theme_classic() + 
                                                         ggplot2::geom_col(fill=cor_grafico) +
                                                         ggplot2::geom_text(aes(label=paste0(Percentage,'%'),y=Frequency+tx1*mean(Frequency)),
                                                                    position = position_nudge(y=0.9),
                                                                    size=size) +
                                                        expand_limits(y=max(table_count_prop['Frequency'])*tx2)
    if(coord_flip == TRUE){
        gg_count_prop <- gg_count_prop + ggplot2::coord_flip()
    }
    

    assign(x=paste0('gg_count_prop',nome_variavel),value=gg_count_prop,envir=.GlobalEnv)
    return( 
        list(paste0('Nome do gráfico: gg_count_prop',nome_variavel)#, 
            #gg_count_prop 
            )
        )
}

## Frequency and Percentage (with geom_label) e axis with angle
grafico_tab_frequencia3 <- function(table_count_prop,nome_variavel,cor_grafico,size=12,angle=45,tx1=0.8,tx2=1.15,
                                    legend=TRUE,percentage=TRUE,count=TRUE){
    gg_count_prop <- ggplot2::ggplot(aes(x=Category,y=Frequency),data=table_count_prop) +  ggplot2::theme_classic() + 
                                                        ggplot2::geom_col(fill=cor_grafico)
    if(legend==TRUE){

        if(percentage==TRUE){
            gg_count_prop <- gg_count_prop + ggplot2::geom_label(aes(label=paste0(Percentage,'%'),
                                                                    y=Frequency+tx1*mean(Frequency)),
                                                                position = position_dodge(0.9),
                                                                #vjust=1.2,
                                                                size=size)
        }

        if(count==TRUE){
            gg_count_prop <- gg_count_prop + ggplot2::geom_text(aes(label=Frequency),
                                            position = position_dodge(0.9),
                                            vjust=-0.1,
                                            size=size)
        }

    }
    
    gg_count_prop <- gg_count_prop +
                        theme(axis.text.x = element_text(angle = angle, hjust=1)) +
                        expand_limits(y=max(table_count_prop['Frequency'])*tx2)
                                                        
    assign(x=paste0('gg_count_prop',nome_variavel),value=gg_count_prop,envir=.GlobalEnv)
    return( 
        list(paste0('Nome do gráfico: gg_count_prop',nome_variavel)#, 
            #gg_count_prop 
            )
        )
}

#------------------------------------------------------------------------------------------------------
# Variável Quantitativa

apply_binning_by_limites_intervalos <- function(limites_intervalos,dados_entrada,nome_variavel){
        dados_saida <- cut(x=dados_entrada[[nome_variavel]],
                            breaks=limites_intervalos,
                            include.lowest=TRUE)
        k <- length(levels(dados_saida))
        dados_saida <- addNA(dados_saida)
        lista <- list(k=k,limites_intervalos=limites_intervalos,dados_saida=dados_saida)
        assign(x=paste0('lista_',nome_variavel),value=lista,envir=.GlobalEnv)
        return( 
        list(paste0('Nome da lista: lista_',nome_variavel), 
            lista
            )
        )
}

apply_binning_by_k <- function(k,dados_entrada,nome_variavel){
        dados_saida <- cut(x=dados_entrada[[nome_variavel]],breaks=k,include.lowest=TRUE)
        levels_saida <- levels(dados_saida)
        limites_intervalos <- c(-Inf,
                                as.numeric( sub("\\((.+),.*", "\\1", levels_saida[-1]) ),
                                Inf)
                                
        levels(dados_saida)[1] <- gsub('.*,','(-Inf,',levels(dados_saida)[1])
        levels(dados_saida)[length(levels(dados_saida))] <- gsub(',.*',',Inf),',levels(dados_saida)[length(levels(dados_saida))])

        lista <- list(k=k,limites_intervalos=limites_intervalos,dados_saida=dados_saida) 
        assign(x=paste0('lista_',nome_variavel),value=lista,envir=.GlobalEnv)
        return( 
        list(paste0('Nome da lista: lista_',nome_variavel), 
            lista
            )
        )
}

binning_quantities <- function(dados_entrada,nome_variavel,method='Percentis'){

    if (method == 'Percentis'){
        percentis <- seq(0.1,0.9,by=.1)
        limites_intervalos <- unname(c(-Inf,
                                    quantile(dados_entrada[[nome_variavel]],probs=percentis,na.rm=TRUE),
                                    Inf))

        apply_binning_by_limites_intervalos(limites_intervalos,dados_entrada,nome_variavel)
    }
    else if(method == 'Sturges'){
        k <- nclass.Sturges(na.omit(dados_entrada[[nome_variavel]]))

        apply_binning_by_k(k,dados_entrada,nome_variavel)

    }
    else if (method == 'Scott') {
        k <- nclass.scott(na.omit(dados_entrada[[nome_variavel]]))

        apply_binning_by_k(k,dados_entrada,nome_variavel)

    }
    else if (method == 'Friedman-Diaconis') {
       k <- nclass.FD(na.omit(dados_entrada[[nome_variavel]]))
       
       apply_binning_by_k(k,dados_entrada,nome_variavel)
    }

    else if (method == 'RegraPratica') {
        if(length(na.omit(dados_entrada[[nome_variavel]])) <= 25){
            k <- 5
        }
        else {
            k <- sqrt(length(na.omit(dados_entrada[[nome_variavel]])))
        }

        apply_binning_by_k(k,dados_entrada,nome_variavel)

    }

    else {
        print('This method does not exist!')
    }

}

