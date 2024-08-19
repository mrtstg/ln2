{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Crud.Template
  ( templatePatchToQuery
  ) where

import           Data.Models.Proxmox.Template
import           Database.Persist
import           Foundation

templatePatchToQuery :: MachineTemplatePatch -> [Update MachineTemplate]
templatePatchToQuery (MachineTemplatePatch { .. }) = let
  nameQ = case getTemplatePatchName of
    ""   -> []
    name -> [MachineTemplateName =. name]
  idQ = case getTemplatePatchId of
    Nothing    -> []
    (Just nid) -> [MachineTemplateProxmoxId =. nid]
  commentQ = case getTemplatePatchComment of
    Nothing        -> []
    (Just comment) -> [MachineTemplateComment =. comment]
  in nameQ ++ idQ ++ commentQ
