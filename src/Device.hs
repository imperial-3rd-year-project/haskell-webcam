module Device (Device (Device), deviceDescription ) where 

-- Barebones declaration for now. To be extended when we see what data needs to be stored
data Device = Device String


deviceDescription :: Device -> String
deviceDescription (Device name) = name

