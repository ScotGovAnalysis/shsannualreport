chapter_2_data <- function(local_authority, year) {

  Table_2.1 <- readRDS("data/dataset/Table 2.1.Rds")
  Table_2.2 <- readRDS("data/dataset/Table 2.2.Rds")
  Table_2.3 <- readRDS("data/dataset/Table 2.3.Rds")
  Figure_2.1 <- readRDS("data/dataset/Figure 2.1.Rds")
  Figure_2.2  <- readRDS("data/dataset/Figure 2.2.Rds")
  Table_2.4_Figure_2.3 <- readRDS("data/dataset/Table 2.4, Figure 2.3.Rds")
  Table_2.5 <- readRDS("data/dataset/Table 2.5.Rds")
  Table_2.6 <- readRDS("data/dataset/Table 2.6.Rds") # type 2
  Figure_2.4 <- readRDS("data/dataset/Figure 2.4.Rds")
  Table_2.7 <- readRDS("data/dataset/Table 2.7.Rds")
  Figure_2.5 <- readRDS("data/dataset/Figure 2.5.Rds")
  Table_2.8  <- readRDS("data/dataset/Table 2.8.Rds")
  Table_2.9  <- readRDS("data/dataset/Table 2.9.Rds")
  Table_2.10 <- readRDS("data/dataset/Table 2.10.Rds") # type 3
  Table_2.11 <- readRDS("data/dataset/Table 2.11.Rds")
  Table_2.12 <- readRDS("data/dataset/Table 2.12.Rds")
  Table_2.13 <- readRDS("data/dataset/Table 2.13.Rds")
  Table_2.14 <- readRDS("data/dataset/Table 2.14.Rds")
  Table_2.15 <- readRDS("data/dataset/Table 2.15.Rds")
  Table_2.16 <- readRDS("data/dataset/Table 2.16.Rds")
  Table_2.17 <- readRDS("data/dataset/Table 2.17.Rds")


  # Table_2.1
  column_names <- colnames(Table_2.1)[!grepl("_l", colnames(Table_2.1)) & !grepl("_u", colnames(Table_2.1))]
  Table_2.1 <- dplyr::select(Table_2.1, column_names)
  Table_2.1 <- Table_2.1[Table_2.1$Council == local_authority,]
  Table_2.1 <- dplyr::select(Table_2.1, -c("Council"))

  # Table_2.2
  column_names <- colnames(Table_2.2)[!grepl("_l", colnames(Table_2.2)) & !grepl("_u", colnames(Table_2.2))]
  Table_2.2 <- dplyr::select(Table_2.2, column_names)
  Table_2.2 <- Table_2.2[Table_2.2$Council == local_authority,]
  Table_2.2 <- dplyr::select(Table_2.2, -c("Council"))

  # Table_2.3
  column_names <- colnames(Table_2.3)[!grepl("_l", colnames(Table_2.3)) & !grepl("_u", colnames(Table_2.3))]
  Table_2.3 <- dplyr::select(Table_2.3, column_names)
  Table_2.3 <- Table_2.3[Table_2.3$Council == local_authority,]
  Table_2.3 <- dplyr::select(Table_2.3, -c("Council"))

  # Figure_2.1
  column_names <- colnames(Figure_2.1)[!grepl("_l", colnames(Figure_2.1)) & !grepl("_u", colnames(Figure_2.1))]
  Figure_2.1 <- dplyr::select(Figure_2.1, column_names)
  Figure_2.1 <- Figure_2.1[Figure_2.1$Council == local_authority,]
  Figure_2.1 <- dplyr::select(Figure_2.1, -c("Council"))

  # Figure_2.2
  column_names <- colnames(Figure_2.2)[!grepl("_l", colnames(Figure_2.2)) & !grepl("_u", colnames(Figure_2.2))]
  Figure_2.2 <- dplyr::select(Figure_2.2, column_names)
  Figure_2.2 <- Figure_2.2[Figure_2.2$Council == local_authority,]
  Figure_2.2 <- dplyr::select(Figure_2.2, -c("Council"))

  # Table_2.4_Figure_2.3
  column_names <- colnames(Table_2.4_Figure_2.3)[!grepl("_l", colnames(Table_2.4_Figure_2.3)) & !grepl("_u", colnames(Table_2.4_Figure_2.3))]
  Table_2.4_Figure_2.3 <- dplyr::select(Table_2.4_Figure_2.3, column_names)
  Table_2.4_Figure_2.3 <- Table_2.4_Figure_2.3[Table_2.4_Figure_2.3$Council == local_authority,]
  Table_2.4_Figure_2.3 <- dplyr::select(Table_2.4_Figure_2.3, -c("Council"))

  # Table_2.5
  column_names <- colnames(Table_2.5)[!grepl("_l", colnames(Table_2.5)) & !grepl("_u", colnames(Table_2.5))]
  Table_2.5 <- dplyr::select(Table_2.5, column_names)
  Table_2.5 <- Table_2.5[Table_2.5$Council == local_authority,]
  Table_2.5 <- dplyr::select(Table_2.5, -c("Council"))

  # Table_2.6 # type 2
  column_names <- colnames(Table_2.6)[!grepl("_l", colnames(Table_2.6)) & !grepl("_u", colnames(Table_2.6))]
  Table_2.6 <- dplyr::select(Table_2.6, column_names)
  Table_2.6 <- Table_2.6[Table_2.6$Council == local_authority & Table_2.6$Year == year,]
  Table_2.6 <- dplyr::select(Table_2.6, -c("Council", "Year"))

  # Figure_2.4
  column_names <- colnames(Figure_2.4)[!grepl("_l", colnames(Figure_2.4)) & !grepl("_u", colnames(Figure_2.4))]
  Figure_2.4 <- dplyr::select(Figure_2.4, column_names)
  Figure_2.4 <- Figure_2.4[Figure_2.4$Council == local_authority,]
  Figure_2.4 <- dplyr::select(Figure_2.4, -c("Council"))

  # Table_2.7
  column_names <- colnames(Table_2.7)[!grepl("_l", colnames(Table_2.7)) & !grepl("_u", colnames(Table_2.7))]
  Table_2.7 <- dplyr::select(Table_2.7, column_names)
  Table_2.7 <- Table_2.7[Table_2.7$Council == local_authority,]
  Table_2.7 <- dplyr::select(Table_2.7, -c("Council"))

  # Figure_2.5
  column_names <- colnames(Figure_2.5)[!grepl("_l", colnames(Figure_2.5)) & !grepl("_u", colnames(Figure_2.5))]
  Figure_2.5 <- dplyr::select(Figure_2.5, column_names)
  Figure_2.5 <- Figure_2.5[Figure_2.5$Council == local_authority,]
  Figure_2.5 <- dplyr::select(Figure_2.5, -c("Council"))

  # Table_2.8
  column_names <- colnames(Table_2.8)[!grepl("_l", colnames(Table_2.8)) & !grepl("_u", colnames(Table_2.8))]
  Table_2.8 <- dplyr::select(Table_2.8, column_names)
  Table_2.8 <- Table_2.8[Table_2.8$Council == local_authority,]
  Table_2.8 <- dplyr::select(Table_2.8, -c("Council"))

  # Table_2.9
  column_names <- colnames(Table_2.9)[!grepl("_l", colnames(Table_2.9)) & !grepl("_u", colnames(Table_2.9))]
  Table_2.9 <- dplyr::select(Table_2.9, column_names)
  Table_2.9 <- Table_2.9[Table_2.9$Council == local_authority,]
  Table_2.9 <- dplyr::select(Table_2.9, -c("Council"))

  # Table_2.10 # type 3
  column_names <- colnames(Table_2.10)[!grepl("_l", colnames(Table_2.10)) & !grepl("_u", colnames(Table_2.10))]
  Table_2.10 <- dplyr::select(Table_2.10, column_names)
  Table_2.10 <- Table_2.10[Table_2.10$Council == local_authority & Table_2.10$Year == year,]
  Table_2.10 <- dplyr::select(Table_2.10, -c("Council", "Year"))

  # Table_2.11
  column_names <- colnames(Table_2.11)[!grepl("_l", colnames(Table_2.11)) & !grepl("_u", colnames(Table_2.11))]
  Table_2.11 <- dplyr::select(Table_2.11, column_names)
  Table_2.11 <- Table_2.11[Table_2.11$Council == local_authority,]
  Table_2.11 <- dplyr::select(Table_2.11, -c("Council"))

  # Table_2.12
  column_names <- colnames(Table_2.12)[!grepl("_l", colnames(Table_2.12)) & !grepl("_u", colnames(Table_2.12))]
  Table_2.12 <- dplyr::select(Table_2.12, column_names)
  Table_2.12 <- Table_2.12[Table_2.12$Council == local_authority,]
  Table_2.12 <- dplyr::select(Table_2.12, -c("Council"))

  # Table_2.13
  column_names <- colnames(Table_2.13)[!grepl("_l", colnames(Table_2.13)) & !grepl("_u", colnames(Table_2.13))]
  Table_2.13 <- dplyr::select(Table_2.13, column_names)
  Table_2.13 <- Table_2.13[Table_2.13$Council == local_authority,]
  Table_2.13 <- dplyr::select(Table_2.13, -c("Council"))

  # Table_2.14
  column_names <- colnames(Table_2.14)[!grepl("_l", colnames(Table_2.14)) & !grepl("_u", colnames(Table_2.14))]
  Table_2.14 <- dplyr::select(Table_2.14, column_names)
  Table_2.14 <- Table_2.14[Table_2.14$Council == local_authority,]
  Table_2.14 <- dplyr::select(Table_2.14, -c("Council"))

  # Table_2.15
  column_names <- colnames(Table_2.15)[!grepl("_l", colnames(Table_2.15)) & !grepl("_u", colnames(Table_2.15))]
  Table_2.15 <- dplyr::select(Table_2.15, column_names)
  Table_2.15 <- Table_2.15[Table_2.15$Council == local_authority,]
  Table_2.15 <- dplyr::select(Table_2.15, -c("Council"))

  # Table_2.16
  column_names <- colnames(Table_2.16)[!grepl("_l", colnames(Table_2.16)) & !grepl("_u", colnames(Table_2.16))]
  Table_2.16 <- dplyr::select(Table_2.16, column_names)
  Table_2.16 <- Table_2.16[Table_2.16$Council == local_authority,]
  Table_2.16 <- dplyr::select(Table_2.16, -c("Council"))

  # Table_2.17
  column_names <- colnames(Table_2.17)[!grepl("_l", colnames(Table_2.17)) & !grepl("_u", colnames(Table_2.17))]
  Table_2.17 <- dplyr::select(Table_2.17, column_names)
  Table_2.17 <- Table_2.17[Table_2.17$Council == local_authority,]
  Table_2.17 <- dplyr::select(Table_2.17, -c("Council"))

  list(Table_2.1,
       Table_2.2,
       Table_2.3,
       Figure_2.1,
       Figure_2.2,
       Table_2.4_Figure_2.3,
       Table_2.5,
       Table_2.6,
       Figure_2.4,
       Table_2.7,
       Figure_2.5,
       Table_2.8,
       Table_2.9,
       Table_2.10,
       Table_2.11,
       Table_2.12,
       Table_2.13,
       Table_2.14,
       Table_2.15,
       Table_2.16,
       Table_2.17
  )

}
