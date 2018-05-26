module Permission.Permission where
import Permission.User
import Element.Element

data PermissionRecord = PermissionRecord ElementId UserId

data PermissionManager = MList PermissionRecord

data Request = Request {
    id :: ElementId
  , from :: UserId
  , to :: UsrId
}

data RequestManager = MQueue Request