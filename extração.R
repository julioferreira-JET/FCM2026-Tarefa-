library(pdftools)
library(stringr)

arquivo <- "cadastro.pdf"
conteudo <- pdftools::pdf_text(arquivo)

conteudo <- str_split(conteudo, "\\n")

conteudo <- Reduce(c, conteudo)
conteudo <- trimws(conteudo, "both")
conteudo <- conteudo[conteudo != ""]

# pegando onde começa cada pessoa
pos_out <- grep("^[Nn]ome:", conteudo)
pos_out <- c(pos_out, length(conteudo) + 1)

run_conteudo_pdf <- function(x, conteudo, pos_out){
    i <- x
    
    # pegando o bloco de texto só do cara da vez
    conteudo_out <- conteudo[pos_out[i]:(pos_out[i+1]-1)]
    
    pos_nome <- grep("^[Nn]ome:", conteudo_out)
    linha_nome <- conteudo_out[pos_nome]
    nome_completo <- str_remove_all(linha_nome, "^[Nn]ome:\\s*")
    
    # prof ensinou esse truque do ?<= pra pegar texto dps de um padrao especifico
    apelido <- str_extract(nome_completo, "(?<=\\(aka ).*(?=\\))")
    
    # tira a parte com parênteses pra sobrar o nome limpo
    nome <- str_remove_all(nome_completo, "\\s*\\(aka.*\\)")
    
    # tem "Data de nascimento" e "Dt nasc" misturado, entao joguei o | pra buscar os dois
    pos_dt <- grep("^[Dd]ata de nascimento:|^[Dd]t nasc:", conteudo_out)
    linha_dt <- conteudo_out[pos_dt]
    dt_nasc <- str_remove_all(linha_dt, "^[Dd]ata de nascimento:\\s*|^[Dd]t nasc:\\s*")
    
    pos_end <- grep("^[Ee]ndereço:", conteudo_out)
    linha_end <- conteudo_out[pos_end]
    
    if(str_detect(linha_end, "CEP:")){
        cep <- str_extract(linha_end, "(?<=CEP:).*")
        endereco <- str_remove_all(linha_end, "\\s*CEP:.*")
        endereco <- str_remove_all(endereco, "^[Ee]ndereço:\\s*")
    } else {
        endereco <- str_remove_all(linha_end, "^[Ee]ndereço:\\s*")
        
        pos_cep <- grep("^[Cc][Ee][Pp]:", conteudo_out)
        linha_cep <- conteudo_out[pos_cep]
        cep <- str_remove_all(linha_cep, "^[Cc][Ee][Pp]:\\s*")
    }
    
    pos_tel <- grep("^[Tt]el.*:", conteudo_out)
    linha_tel <- conteudo_out[pos_tel]
    telefone <- str_remove_all(linha_tel, "^[Tt]el.*:\\s*")
    
    pos_cpf <- grep("^[Cc][Pp][Ff]:", conteudo_out)
    linha_cpf <- conteudo_out[pos_cpf]
    cpf <- str_remove_all(linha_cpf, "^[Cc][Pp][Ff]:\\s*")
    
    nome <- trimws(nome, "both")
    apelido <- trimws(apelido, "both")
    dt_nasc <- trimws(dt_nasc, "both")
    endereco <- trimws(endereco, "both")
    cep <- trimws(cep, "both")
    telefone <- trimws(telefone, "both")
    cpf <- trimws(cpf, "both")
    
    # montando o df da pessoa
    info <- data.frame(
        Nome = nome, 
        Apelido = apelido, 
        Data_Nascimento = dt_nasc, 
        Endereco = endereco, 
        CEP = cep, 
        Telefone = telefone, 
        CPF = cpf
    )
    
    return(info)
}

# usando lapply igual no script base pra ler a lista toda
dados <- lapply(1:(length(pos_out)-1), run_conteudo_pdf, conteudo = conteudo, pos_out = pos_out)

# empilhando tudo num df unico
dados <- Reduce(rbind, dados)

  dados

openxlsx::write.xlsx(dados, "resultado_extracao.xlsx")