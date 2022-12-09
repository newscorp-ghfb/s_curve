Public Sub SaveAttachmentsToDisk(MItem As Outlook.MailItem)

    Dim oAttachment As Outlook.Attachment
    Dim sSaveFolder As String

    sSaveFolder = "C:\Users\glen.falk\OneDrive - IHS Markit\Documents\OPIS\volume_rpt\"

    For Each oAttachment In MItem.Attachments
        oAttachment.SaveAsFile sSaveFolder & oAttachment.DisplayName
    Next

End Sub