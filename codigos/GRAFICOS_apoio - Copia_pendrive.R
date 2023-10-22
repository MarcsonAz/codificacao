# codificacao da coleta
# Pesquisa: Percepção sobre o Trabalho Remoto e/ou Híbrido
# Pesquisadores: Juliene Saback, Leonardo Lopes, Lucas Simões e Marcson Araújo

# ./codigos/02_GRAFICOS.R
# GRÁFICOS PARA CONSTRUÇÃO DO ARTIGO


cor_mulher = brewer.pal(n = 5, name = 'Blues')[3]
cor_homem = brewer.pal(n = 5, name = 'Blues')[5]

plot_at1 = df_concilia_at %>% 
  pivot_longer(cols = starts_with('n'),
               names_to = 'atividades',
               values_to = 'frequencia') %>% 
  
  filter(atividades == 'n_at1') %>% 
  
  ggplot(aes(x=atividades, y = frequencia, fill=sexo)) + 
  geom_col() + #position="stack", stat="identity") +
  labs(title = paste0("Caso concilie/conciliou tarefas domésticas com a sua rotina de trabalho remoto, que tipo de tarefas costuma exercer?
<br><span style='font-size:11pt'> Segundo sexo: <i style='color:",
                      cor_mulher,"'>Mulheres</i> e <i style='color:",
                      cor_homem,"'>Homens</i></span><br><br>Preparar ou servir alimentos, arrumar a mesa ou lavar louça")) +
    
  #   "O que <i style='color:",
  #                     cor_mulher,"'>Mulheres</i><i style='color:",
  #                     cor_homem,"'>Homens</i> e    fazem em casa quando conciliam atividades domésticas com o trabalho remoto<br>Segundo o sexo: <i style='color:",
  #                     cor_homem,"'>Homens</i> e <i style='color:",
  #                     cor_mulher,"'>Mulheres</i>  ")) +
  scale_colour_manual(values = c(cor_mulher,cor_homem),
  aesthetics = c("colour", "fill")) + 
  coord_flip() + 
  theme_void() + 
  guides(fill="none") +
  theme(
    plot.title = element_markdown(
      lineheight = 1.1, vjust = -0.1, hjust = 0.05),
    legend.text = element_markdown(size = 11),
    #plot.caption = element_markdown(size = 11)
  )


plot_at2 = df_concilia_at %>% 
  pivot_longer(cols = starts_with('n'),
               names_to = 'atividades',
               values_to = 'frequencia') %>% 
  
  filter(atividades == 'n_at2') %>% 
  
  ggplot(aes(x=atividades, y = frequencia, fill=sexo)) + 
  geom_col() + #position="stack", stat="identity") +
  labs(title = paste0("Cuidar da limpeza ou manutenção de roupas e sapatos<br>Segundo o sexo: <i style='color:",
                      cor_homem,"'>Homens</i> e <i style='color:",
                      cor_mulher,"'>Mulheres</i>")) +
  scale_colour_manual(values = c(cor_mulher,cor_homem),
                      aesthetics = c("colour", "fill")) + 
  coord_flip() + 
  theme_void() + 
  guides(fill="none") +
  theme(
    plot.title = element_markdown(
      lineheight = 1.1, vjust = -0.1, hjust = 0.05),
    legend.text = element_markdown(size = 11),
    #plot.caption = element_markdown(size = 11)
  )

plot_at3 = df_concilia_at %>% 
  pivot_longer(cols = starts_with('n'),
               names_to = 'atividades',
               values_to = 'frequencia') %>% 
  
  filter(atividades == 'n_at3') %>% 
  
  ggplot(aes(x=atividades, y = frequencia, fill=sexo)) + 
  geom_col() + #position="stack", stat="identity") +
  labs(title = paste0("Fazer pequenos reparos ou manutenção do domicílio, do automóvel, de eletrodomésticos<br>Segundo o sexo: <i style='color:",
                      cor_homem,"'>Homens</i> e <i style='color:",
                      cor_mulher,"'>Mulheres</i>")) +
  scale_colour_manual(values = c(cor_mulher,cor_homem),
                      aesthetics = c("colour", "fill")) + 
  coord_flip() + 
  theme_void() + 
  guides(fill="none") +
  theme(
    plot.title = element_markdown(
      lineheight = 1.1, vjust = -0.1, hjust = 0.05)
  )

plot_at4 = df_concilia_at %>% 
  pivot_longer(cols = starts_with('n'),
               names_to = 'atividades',
               values_to = 'frequencia') %>% 
  
  filter(atividades == 'n_at4') %>% 
  
  ggplot(aes(x=atividades, y = frequencia, fill=sexo)) + 
  geom_col() + #position="stack", stat="identity") +
  labs(title = paste0("Limpar ou arrumar o domicílio, a garagem, o quintal ou o jardim<br>Segundo o sexo: <i style='color:",
                      cor_homem,"'>Homens</i> e <i style='color:",
                      cor_mulher,"'>Mulheres</i>")) +
  scale_colour_manual(values = c(cor_mulher,cor_homem),
                      aesthetics = c("colour", "fill")) + 
  coord_flip() + 
  theme_void() + 
  guides(fill="none") +
  theme(
    plot.title = element_markdown(
      lineheight = 1.1, vjust = -0.1, hjust = 0.05)
  )

plot_at5 = df_concilia_at %>% 
  pivot_longer(cols = starts_with('n'),
               names_to = 'atividades',
               values_to = 'frequencia') %>% 
  
  filter(atividades == 'n_at5') %>% 
  
  ggplot(aes(x=atividades, y = frequencia, fill=sexo)) + 
  geom_col() + #position="stack", stat="identity") +
  labs(title = paste0("Cuidar da organização do domicílio<br>Segundo o sexo: <i style='color:",
                      cor_homem,"'>Homens</i> e <i style='color:",
                      cor_mulher,"'>Mulheres</i>")) +
  scale_colour_manual(values = c(cor_mulher,cor_homem),
                      aesthetics = c("colour", "fill")) + 
  coord_flip() + 
  theme_void() + 
  guides(fill="none") +
  theme(
    plot.title = element_markdown(
      lineheight = 1.1, vjust = -0.1, hjust = 0.05)
  )

plot_at6 = df_concilia_at %>% 
  pivot_longer(cols = starts_with('n'),
               names_to = 'atividades',
               values_to = 'frequencia') %>% 
  
  filter(atividades == 'n_at6') %>% 
  
  ggplot(aes(x=atividades, y = frequencia, fill=sexo)) + 
  geom_col() + #position="stack", stat="identity") +
  labs(title = paste0("Fazer compras ou pesquisar preços de bens para o domicílio<br>Segundo o sexo: <i style='color:",
                      cor_homem,"'>Homens</i> e <i style='color:",
                      cor_mulher,"'>Mulheres</i>")) +
  scale_colour_manual(values = c(cor_mulher,cor_homem),
                      aesthetics = c("colour", "fill")) + 
  coord_flip() + 
  theme_void() + 
  guides(fill="none") +
  theme(
    plot.title = element_markdown(
      lineheight = 1.1, vjust = -0.1, hjust = 0.05)
  )

plot_at7 = df_concilia_at %>% 
  pivot_longer(cols = starts_with('n'),
               names_to = 'atividades',
               values_to = 'frequencia') %>% 
  
  filter(atividades == 'n_at7') %>% 
  
  ggplot(aes(x=atividades, y = frequencia, fill=sexo)) + 
  geom_col() + #position="stack", stat="identity") +
  labs(title = paste0("Cuidar dos animais domésticos<br>Segundo o sexo: <i style='color:",
                      cor_homem,"'>Homens</i> e <i style='color:",
                      cor_mulher,"'>Mulheres</i>")) +
  scale_colour_manual(values = c(cor_mulher,cor_homem),
                      aesthetics = c("colour", "fill")) + 
  coord_flip() + 
  theme_void() + 
  guides(fill="none") +
  theme(
    plot.title = element_markdown(
      lineheight = 1.1, vjust = -0.1, hjust = 0.05)
  )

plot_at8 = df_concilia_at %>% 
  pivot_longer(cols = starts_with('n'),
               names_to = 'atividades',
               values_to = 'frequencia') %>% 
  
  filter(atividades == 'n_at8') %>% 
  
  ggplot(aes(x=atividades, y = frequencia, fill=sexo)) + 
  geom_col() + #position="stack", stat="identity") +
  labs(title = paste0("Cuidados com outros moradores<br>Segundo o sexo: <i style='color:",
                      cor_homem,"'>Homens</i> e <i style='color:",
                      cor_mulher,"'>Mulheres</i>")) +
  scale_colour_manual(values = c(cor_mulher,cor_homem),
                      aesthetics = c("colour", "fill")) + 
  coord_flip() + 
  theme_void() + 
  guides(fill="none") +
  theme(
    plot.title = element_markdown(
      lineheight = 1.1, vjust = -0.1, hjust = 0.05)
  )



plot_at1 /plot_at2 / plot_at3 / plot_at4 / plot_at5 /plot_at6 / plot_at7 / plot_at8

ggsave("./graficos/plot_conciliar_atividades_sexo.png",
       width = 50, height = 40, units = "cm")


# apoio





library(ggplot2)
library(ggtext)

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(size = 3) +
  labs(
    title = "**Fisher's *Iris* dataset**  
    <span style='font-size:11pt'>Sepal width vs. sepal length for three *Iris*
    species</span>",
    x = "Sepal length (cm)", y = "Sepal width (cm)",
    caption = "<b>Source</b>: *Fisher's Iris dataset*"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(lineheight = 1.1),
    legend.text = element_markdown(size = 11),
    plot.caption = element_markdown(size = 11)
  )