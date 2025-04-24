
# Curso: Introdução à Ciência de Dados com R

Bem-vindo(a) ao repositório da **Aula 01** do curso *Introdução à Ciência de Dados com R*, ministrado por Nelson Quesado e Caio Guimarães. Este espaço concentra os arquivos, scripts e apresentações utilizadas na aula introdutória, com foco no uso da linguagem R para análise de dados.

## 📚 Objetivo da aula

Apresentar os primeiros conceitos de ciência de dados, manipulação e visualização de dados com R, utilizando como caso motivador um estudo sobre mortalidade por armas de fogo nos EUA.

## 🗂 Conteúdo disponível

- `slides.v1.pdf`: Apresentação utilizada em aula
- `aula 01 - introducao.R`: Script com todos os comandos explorados na aula, organizados por seção
- `data.csv`: Conjunto de dados com informações sobre mortes por arma de fogo por estado
- `meu.primeiro.plot.png` e `meu.primeiro.plot.jpg`: Exemplos de visualizações geradas com `ggplot2`

## ▶️ Como usar

1. **Instale o R** e o **RStudio**
2. Faça o clone ou download deste repositório:
   ```bash
   git clone https://github.com/nelsonquesado/curso-introducao-r.git
   ```
3. Abra o script `aula 01 - introducao.R` no RStudio
4. Siga as instruções linha a linha, explorando os conceitos e comandos apresentados

## 📦 Pacotes utilizados

A aula utiliza principalmente os seguintes pacotes do R:
- `tidyverse`: Para leitura, manipulação e visualização de dados
- `ggthemes`: Para temas gráficos adicionais no `ggplot2`

Instale-os com:

```r
install.packages(c("tidyverse", "ggthemes"))
```

## 💡 Dica

Os gráficos estão em escala logarítmica para facilitar a visualização comparativa entre estados com populações muito distintas. Explore os filtros, agregações e temas sugeridos no script e nos slides para adaptar as análises às suas perguntas.

## 🧠 Inspiração

> "A informação no mundo dobra a cada 20 meses."  
> — Thomas Runkle (2012)

---

Bons estudos e bem-vindo(a) ao mundo da ciência de dados com R!
