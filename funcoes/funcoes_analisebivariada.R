#------------------------------------------------------------------------------------------------------
# Variável Qualitativa X Variável Qualitativa
calcula_tab_frequencia_bivariada <- function(tabela_de_entrada,nome_variavel1,nome_variavel2){
    tabela_freq <- as.data.frame(table(tabela_de_entrada[[nome_variavel1]],tabela_de_entrada[[nome_variavel2]]))
    names(tabela_freq) <- c(nome_variavel1,nome_variavel2,'Frequency')
    levels_tabelafreq <- levels(tabela_freq[[nome_variavel2]])

    Sumgroup <- tapply(tabela_freq[['Frequency']],
                       tabela_freq[[nome_variavel2]],
                       FUN=sum)
    Sumgroup <- tibble::enframe(Sumgroup)
    colnames(Sumgroup) <- c(nome_variavel2,'sum_x')
    Sumgroup[['cumsum_sum_x']] <- cumsum(Sumgroup[['sum_x']]) 
    Sumgroup[['percentage_sum_x']] <- round((Sumgroup[['sum_x']]/sum(Sumgroup[['sum_x']]))*100,1)
    
    tabela_freq <- left_join(x=tabela_freq,
                             y=Sumgroup,
                             by=nome_variavel2)
    tabela_freq[[nome_variavel2]] <- factor(tabela_freq[[nome_variavel2]],
                                               levels=levels_tabelafreq)
    tabela_freq[['Frequency_div_sum_x']] <- tabela_freq[['Frequency']]/tabela_freq[['sum_x']]
    tabela_freq[['percentage_Frequency_div_sum_x']] <- round((tabela_freq[['Frequency']]/tabela_freq[['sum_x']])*100,1)
    
    assign(x=paste0('table_count_prop',nome_variavel1,'_',nome_variavel2),value=tabela_freq,envir=.GlobalEnv)
    
    return( 
        list(
            paste0('Nome da tabela abaixo: table_count_prop',nome_variavel1,'_',nome_variavel2), 
            tabela_freq 
        )
    )
}

grafico_tab_frequencia_bivariada2 <- function(table_count_prop,nome_variavel1,nome_variavel2,
                                                tx1=0.8,tx2=1.15,size=4,
                                                legend=TRUE,percentage=TRUE,count=TRUE){
    if(legend == TRUE){
        print('Para retirar as porcentagens, use legend = FALSE ou size = 0!')
    } else if(legend == FALSE){
        print('Para adicionar as porcentagens, use legend = TRUE e size > 0!')
    }
    
    gg_count_prop <- ggplot(aes_string(x=nome_variavel2,y='Frequency',fill=nome_variavel1),
                           data=table_count_prop) +
                        theme_classic() +
                        geom_col(position="dodge") #+
    if(legend==TRUE){
        if(percentage==TRUE){
        gg_count_prop <- gg_count_prop +
                    geom_text(aes(label=paste0(percentage_Frequency_div_sum_x,'%'),
                                y=Frequency+tx1*mean(Frequency)),
                            position = position_dodge(width = 0.9),
                            size=size)
        }
        if(count==TRUE){
        gg_count_prop <- gg_count_prop + 
                geom_text(aes(label=paste0('\n',Frequency),
                            y=Frequency),
                    size=size, 
                    position = position_dodge(width=0.9))
        }

    }

    gg_count_prop <- gg_count_prop +
                expand_limits(y=max(table_count_prop['Frequency'])*tx2) +
                theme(legend.position="right")#+
            # coord_cartesian(xlim=c(table_count_prop[[nome_variavel2]][1],
            #                       table_count_prop[[nome_variavel2]][15]))
            # coord_flip(xlim=c(rev(table_count_prop[[nome_variavel2]])[15],
            #                   rev(table_count_prop[[nome_variavel2]])[1]))
    
    assign(x=paste0('gg_count_prop',nome_variavel1,'_',nome_variavel2),value=gg_count_prop,envir=.GlobalEnv)
    return( 
        list(paste0('Nome do gráfico: gg_count_prop',nome_variavel1,'_',nome_variavel2))#, 
             #gg_count_prop 
        )
}


grafico_tab_frequencia_bivariada3 <- function(table_count_prop,nome_variavel1,nome_variavel2,size=2,tx1=0.025,legend=FALSE){
    gg_count_prop <- ggplot(aes_string(x=nome_variavel2,
                                       y='Frequency',
                                       fill=nome_variavel1),
                            data=table_count_prop) +
        theme_classic() +
        geom_col(position='fill') +
        theme(legend.position = "bottom") #+
    if(legend==TRUE){
        gg_count_prop <- gg_count_prop +
                geom_text(aes(label=paste0(percentage_Frequency_div_sum_x,'%'),
                              y=Frequency_div_sum_x+tx1*mean(Frequency_div_sum_x)),
                          position = position_stack(vjust = 0.5),
                          size=size) +
                theme(legend.position="right")#+
    }
    assign(x=paste0('gg_count_prop',nome_variavel1,'_',nome_variavel2),value=gg_count_prop,envir=.GlobalEnv)
    return( 
        list(paste0('Nome do gráfico: gg_count_prop',nome_variavel1,'_',nome_variavel2))#, 
        #gg_count_prop 
    )
}




