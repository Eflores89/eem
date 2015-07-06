#' Read xml invoices
#'
#' Provides interface to parse information embedded in xml invoices emitted in Mexico.
#' SAT (tax authority) provides a data definition to be followed by all invoices. 
#' 
#' @param file .xml file to parse
#' @param extend Do you want addition information? Default = FALSE. TRUE brings RFC (Tax ID Number), Name, IVA (VAT Tax), Date, Month, Year, Total, Folio, Directory.
#' @param spanish Do you want column names in Spanish? Default = FALSE.
#' 
#' @keywords eem
#' @importFrom XML xmlToList
#' @importFrom lubridate to.Date
#' @export
#' @return Data.frame
#' @author Eduardo Flores
#' @examples
#' library(eem)
#' ggplot(data = df, aes(x=Categoria,y=Dato))+
#' geom_point()+
#' theme_eem()
#' 

#NOT RUN YET

factura<-function(file,extend=FALSE,spanish=FALSE)
{
  d<-xmlToList(x)
  
  # get rfc
  rfc<-d$Emisor$.attrs["rfc"]
  ## Name
  nombre<-d$Emisor$.attrs["nombre"]
  ## IVA
  iva<-tryCatch(
    (
    if(is.null(d$Impuestos$.attrs["totalImpuestosTrasladados"]))
      {0} else {d$Impuestos$.attrs["totalImpuestosTrasladados"]}
      ),
      warning=function(warn) {print(0)},
      error=function(err){print(0)},
      finally={
      #no error}
        ) 
  ## Date
  date<-as.Date(d$.attrs["fecha"][1])
  ## Month
  month<-month(date)
  ## Year
  year<-year(date)
  ## Total
  total<-if(is.null(d$.attrs["total"][1]))
          {0} else {
      total<-d$.attrs["total"][1]
      }
  ## Folio
  folio<-if(is.null(d$.attrs["folio"][1]))
          {0} else {folio<-d$.attrs["folio"][1]
      }
  
  dfm<-data.frame(cbind.data.frame(
                      rfc,
                      nombre,
                      iva,
                      date,
                      month,
                      year,
                      total,
                      folio,
                      directory = x),stringsAsFactors = FALSE)
  #export
  if(extend)
    {
    
    #add description 
    if(is.null(as.list(d$Conceptos$Concepto["descripcion"])$descripcion))
    {
      descr<-"No description"
    } else {descr<-as.list(d$Conceptos$Concepto["descripcion"])$descripcion }
    
    #add address (emision)
      #street
      if (is.null(as.list(d$Emisor$DomicilioFiscal)))
      {
        df_a<-cbind.data.frame(
          street = "No data",
          postalcode = "No data",
          neighborhood = "No data",
          state = "No data",
          municipality = "No data",
          number = "No data",
          country = "No data"
        )
      } else {
        #make list
        l<-as.list(d$Emisor$DomicilioFiscal)
        df_a<-cbind.data.frame(
          street = l$calle,
          postalcode = l$codigoPostal,
          neighborhood = l$colonia,
          state = l$estado,
          municipality = l$municipio,
          number = l$noExterior,
          country = l$pais
        )
      }
    #change names in data.frame 
    
    #add address (receiver)
    
    if (is.null(as.list(d$Receptor$Domicilio)))
    {
      df_a2<-cbind.data.frame(
        street = "No data",
        postalcode = "No data",
        neighborhood = "No data",
        state = "No data",
        municipality = "No data",
        number = "No data",
        country = "No data"
      )
    } else {
      #make list
      l2<-as.list(d$Emisor$DomicilioFiscal)
      df_a2<-cbind.data.frame(
        street = l$calle,
        postalcode = l$codigoPostal,
        neighborhood = l$colonia,
        state = l$estado,
        municipality = l$municipio,
        number = l$noExterior,
        country = l$pais
      )
    }
    
  
  #join all in new data.frame
    dfm2<-cbind.data.frame(
      
    )
    
    #export all
    return(dfm2)
  } else {
    
    #no other descriptors
    return(dfm)
    }
}
