;;; unityjs-mode.el --- Major mode for editing Unity JavaScript

;; Author: Juan Sebastian Muñoz <naruse@gmail.com>

;; Identation took from: http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/lisp/progmodes/js.el?root=emacs
;; Authors: 
;;         Karl Landstrom <karl.landstrom@brgeight.se>
;;         Daniel Colascione <dan.colascione@gmail.com>
;;
;; Version: 1
;; Date: 2010-01-08
;; Keywords: Unity3D, Unity Javascript.

;; This file is licensed as GPL, you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file comes WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.


(defvar unityjs-mode-hook nil)
(defvar unityjs-mode-map
  (let ((unityjs-mode-map (make-sparse-keymap)))
    (define-key unityjs-mode-map "\C-j" 'newline-and-indent)
    unityjs-mode-map)
  "Keymap for UnityJS major mode")

(add-to-list 'auto-mode-alist '("\\.js\\'" . unityjs-mode))

;; Word Highlighting

 (defconst unityjs-font-lock-keywords-1
   (list
    '("\\<\\(A\\(?:bs\\|c\\(?:ceptDrag\\|os\\)\\|dd\\(?:BinaryData\\|C\\(?:lip\\|o\\(?:lor\\|mponent\\(?:Menu\\)?\\|ntrol\\)\\|ursorRect\\)\\|DefaultControl\\|E\\(?:vent\\|xplosionForce\\)\\|F\\(?:ield\\|loat\\|orce\\(?:AtPosition\\)?\\)\\|Key\\|M\\(?:atrix\\|ixingTransform\\)\\|ObjectToAsset\\|Relative\\(?:\\(?:Forc\\|Torqu\\)e\\)\\|Torque\\|Vector\\)?\\|l\\(?:locateViewID\\|tDirectorySeparatorChar\\)\\|n\\(?:gle\\(?:Axis\\)?\\|i\\(?:mation\\(?:BlendMode\\(?:\\.\\(?:Additive\\|Blend\\)\\)?\\|C\\(?:lip\\(?:CurveData\\)?\\|urve\\)\\|Event\\|State\\|Utility\\)?\\|sotropicFiltering\\(?:\\.\\(?:\\(?:Dis\\|\\(?:Force\\)?En\\)able\\)\\)?\\)\\)\\|pp\\(?:l\\(?:ication\\|y\\(?:ModifiedProperties\\)?\\)\\|roximately\\)\\|rr\\(?:ay\\|owCap\\)\\|s\\(?:in\\|set\\(?:Bundle\\(?:Request\\)?\\|Database\\|Importer\\|P\\(?:athToGUID\\|ostprocessor\\)\\)\\|yncOperation\\)\\|tan2?\\|udio\\(?:Clip\\|Importer\\(?:Format\\(?:\\.\\(?:Compressed\\|Native\\)\\)?\\)?\\|Listener\\|Source\\|VelocityUpdateMode\\(?:\\.\\(?:Auto\\|Dynamic\\|Fixed\\)\\)?\\)\\|wake\\)\\|B\\(?:e\\(?:ep\\|gin\\(?:Area\\|G\\(?:UI\\|roup\\)\\|Horizontal\\|LayoutGroup\\|S\\(?:ample\\|crollView\\)\\|ToggleGroup\\|Vertical\\|Windows\\)?\\|haviour\\)\\|itStream\\|l\\(?:end\\|it\\(?:MultiTap\\)?\\)\\|o\\(?:neWeight\\|unds\\|x\\(?:Collider\\)?\\)\\|r\\(?:eak\\|ingWindowTo\\(?:Back\\|Front\\)\\|o\\(?:adcastMessage\\|wseURL\\)\\)\\|u\\(?:ild\\(?:AssetBundle\\(?:ExplicitAssetNames\\|Options\\(?:\\.\\(?:Co\\(?:\\(?:llectDependencie\\|mpleteAsset\\)s\\)\\|DeterministicAssetBundle\\)\\)?\\)?\\|Options\\(?:\\.\\(?:AutoRunPlayer\\|BuildAdditionalStreamedScenes\\|Development\\|ShowBuiltPlayer\\)\\)?\\|P\\(?:ipeline\\|layer\\)\\|Target\\(?:\\.\\(?:DashboardWidget\\|Standalone\\(?:OSX\\(?:Intel\\|PPC\\|Universal\\)\\|Windows\\)\\|WebPlayer\\(?:Streamed\\)?\\)\\)?\\)\\|tton\\)\\)\\|C\\(?:a\\(?:l\\(?:c\\(?:Height\\|LineTranslation\\|MinMaxWidth\\|S\\(?:\\(?:creenS\\)?ize\\)\\|ulate\\(?:FrustumPlanes\\|TransformPath\\)\\)\\|lbackFunction\\)\\|mera\\(?:ClearFlags\\(?:\\.\\(?:Depth\\|Nothing\\|S\\(?:kybox\\|olidColor\\)\\)\\)?\\)?\\|n\\(?:StreamedLevelBeLoaded\\|cel\\(?:Invoke\\|Quit\\)\\)\\|p\\(?:suleCollider\\|tureScreenshot\\)\\)\\|eil\\(?:ToInt\\)?\\|h\\(?:aracter\\(?:Controller\\|Joint\\)\\|eck\\(?:\\(?:Capsul\\|Spher\\)e\\)\\)\\|l\\(?:amp\\(?:01\\)?\\|ear\\(?:C\\(?:amera\\|urves\\)\\|HostList\\|Log\\|P\\(?:articles\\|rogressBar\\)\\|SnapshotTarget\\)?\\|o\\(?:neComponent\\|se\\(?:Connection\\|stPo\\(?:int\\(?:OnBounds\\|To\\(?:Arc\\|Disc\\|PolyLine\\)\\)\\|werOfTwo\\)\\)?\\)\\)\\|o\\(?:l\\(?:l\\(?:ectDe\\(?:epHierarchy\\|pendencies\\)\\|i\\(?:der\\|sion\\(?:Flags\\(?:\\.\\(?:Above\\|Below\\|None\\|Sides\\)\\)?\\)?\\)\\)\\|or\\(?:Field\\)?\\)\\|m\\(?:bine\\(?:Instance\\|Meshes\\)?\\|p\\(?:areTag\\|onent\\|ress\\(?:Texture\\)?\\)\\)\\|n\\(?:cat\\|eCap\\|figurableJoint\\(?:Motion\\(?:\\.\\(?:Free\\|L\\(?:\\(?:imit\\|ock\\)ed\\)\\)\\)?\\)?\\|nect\\(?:ionTesterStatus\\(?:\\.\\(?:Error\\|P\\(?:rivateIP\\(?:\\(?:HasNATPunchT\\|NoNATPuncht\\)hrough\\)\\|ublicIP\\(?:IsConnectable\\|\\(?:NoServerStart\\|PortBlock\\)ed\\)\\)\\|Undetermined\\)\\)?\\)?\\|stantForce\\|t\\(?:a\\(?:ctPoint\\|ins\\(?:Key\\|Value\\)?\\)\\|extMenu\\|rollerColliderHit\\)\\)\\|py\\(?:Asset\\|F\\(?:ileOrDirectory\\(?:FollowSymlinks\\)?\\|rom\\)\\|PropertiesFromMaterial\\|Serialized\\)?\\|routine\\|s\\|untRemaining\\)\\|r\\(?:eate\\(?:Asset\\|EmptyPrefab\\|GameObjectWithHideFlags\\|Instance\\|Primitive\\|\\(?:Snapsho\\|TerrainGameObjec\\)t\\)?\\|oss\\(?:Fade\\(?:Queued\\)?\\)?\\)\\|u\\(?:be\\(?:Cap\\|map\\(?:Face\\(?:\\.\\(?:Negative[XYZ]\\|Positive[XYZ]\\)\\)?\\)?\\)\\|stomEditor\\)\\|ylinderCap\\)\\|D\\(?:e\\(?:bug\\|creaseLevel\\|g2Rad\\|l\\(?:ete\\(?:A\\(?:ll\\|sset\\)\\|\\(?:FileOrDirector\\|Ke\\)y\\)\\|taAngle\\)\\|pthTextureMode\\(?:\\.\\(?:Depth\\(?:Normals\\)?\\|None\\)\\)?\\|stroy\\(?:Immediate\\|PlayerObjects\\)?\\|tachChildren\\)\\|i\\(?:rectorySeparatorChar\\|s\\(?:c\\(?:onnect\\)?\\|play\\(?:Dialog\\(?:Complex\\)?\\|P\\(?:opupMenu\\|rogressBar\\)\\|Wizard\\)\\|tance\\(?:PointLine\\|To\\(?:Arc\\|Circle\\|Disc\\|\\(?:Poly\\)?Line\\)\\)?\\)\\)\\|o\\(?:ntDestroyOnLoad\\|t\\(?:Cap\\)?\\)\\|r\\(?:a\\(?:g\\(?:AndDrop\\(?:VisualMode\\(?:\\.\\(?:Copy\\|Generic\\|Link\\|\\(?:Mov\\|Non\\)e\\)\\)?\\)?\\|Window\\)\\|w\\(?:A\\(?:APolyLine\\|rrow\\)\\|C\\(?:a\\(?:mera\\|pFunction\\)\\|o\\(?:lorSwatch\\|ne\\)\\|u\\(?:be\\|rsor\\)\\|ylinder\\)\\|DefaultInspector\\|G\\(?:UITexture\\|izmo\\)\\|Icon\\|Line\\|Mesh\\(?:Now\\)?\\|PolyLine\\|R\\(?:ay\\|ectangle\\)\\|S\\(?:olid\\(?:\\(?:Ar\\|Dis\\)c\\)\\|phere\\)\\|Texture\\(?:Alpha\\)?\\|Wi\\(?:re\\(?:Arc\\|Cube\\|Disc\\|Sphere\\)\\|thTextSelection\\)\\)?\\)\\|opShadowLabel\\)\\)\\|E\\(?:aseInOut\\|ditor\\(?:Application\\|GUI\\(?:Layout\\|Utility\\)?\\|Prefs\\|Styles\\|Utility\\|Window\\)?\\|mit\\|n\\(?:c\\(?:apsulate\\|odeToPNG\\)\\|d\\(?:Area\\|G\\(?:UI\\|roup\\)\\|Horizontal\\|S\\(?:ample\\|crollView\\)\\|ToggleGroup\\|Vertical\\|Windows\\)?\\|umPopup\\)\\|psilon\\|scapeURL\\|uler\\|v\\(?:aluate\\|ent\\(?:Type\\(?:\\.\\(?:ContextClick\\|Drag\\(?:Exited\\|Perform\\|Updated\\)\\|ExecuteCommand\\|Ignore\\|Key\\(?:Down\\|Up\\)\\|Layout\\|Mouse\\(?:D\\(?:own\\|rag\\)\\|Move\\|Up\\)\\|Repaint\\|ScrollWheel\\|\\(?:Use\\|ValidateComman\\)d\\)\\)?\\)?\\)\\|x\\(?:ecute\\(?:InEditMode\\|M\\(?:acro\\|enuItem\\)\\)\\|it\\|p\\(?:and\\(?:Height\\|Width\\)?\\)?\\|t\\(?:ernal\\(?:\\(?:Cal\\|Eva\\)l\\)\\|ractOggFile\\)\\)\\)\\|F\\(?:i\\(?:l\\(?:eUtil\\|terMode\\(?:\\.\\(?:Bilinear\\|Point\\|Trilinear\\)\\)?\\)\\|nd\\(?:GameObjectsWithTag\\|Object\\(?:s?OfType\\)\\|Pr\\(?:efabRoot\\|operty\\)\\|Style\\|Texture\\|WithTag\\)?\\|xed\\(?:Joint\\|Update\\)\\)\\|l\\(?:are\\|exibleSpace\\|o\\(?:atField\\|or\\(?:ToInt\\)?\\)\\)\\|o\\(?:cus\\(?:Control\\|ProjectWindow\\|Type\\(?:\\.\\(?:Keyboard\\|\\(?:Nat\\|Pass\\)ive\\)\\)?\\|Window\\)?\\|ldout\\|nt\\(?:RenderMode\\(?:\\.\\(?:\\(?:Light\\|No\\|Strong\\)Antialiasing\\)\\)?\\|TextureCase\\(?:\\.\\(?:ASCII\\(?:\\(?:Low\\|Upp\\)erCase\\)?\\|Unicode\\)\\)?\\)?\\|r\\(?:ceMode\\(?:\\.\\(?:Acceleration\\|\\(?:Forc\\|Impuls\\|VelocityChang\\)e\\)\\)?\\|matBytes\\)\\)\\|r\\(?:ee\\(?:\\(?:Mov\\|Rotat\\)eHandle\\)\\|omToRotation\\)\\)\\|G\\(?:L\\|UI\\(?:Content\\|DToAssetPath\\|Element\\|Lay\\(?:er\\|out\\(?:Option\\|Utility\\)?\\)\\|PointToWorldRay\\|S\\(?:ettings\\|kin\\|tyle\\(?:State\\)?\\)\\|T\\(?:ext\\(?:ure\\)?\\|oScreenPoint\\)\\|Utility\\)?\\|ameObject\\|e\\(?:nerateUniqueAssetPath\\|ometryUtility\\|t\\(?:A\\(?:llCurves\\|nimat\\(?:able\\(?:Objects\\|Properties\\(?:ForObject\\)?\\)\\|ion\\(?:\\(?:Clip\\|Event\\)s\\)\\)\\|s\\(?:pectRect\\|setPath\\)\\|tPath\\|veragePing\\|xis\\(?:Raw\\)?\\)\\|B\\(?:ool\\|utton\\(?:Down\\|Up\\)?\\)\\|C\\(?:achedIcon\\|l\\(?:ass\\(?:Name\\)?\\|ipCount\\)\\|o\\(?:l\\(?:or\\|umn\\)\\|mponent\\(?:InChildren\\|s\\(?:InChildren\\)?\\)?\\|ntrolID\\)\\|ursor\\(?:PixelPosition\\|StringIndex\\)\\)\\|D\\(?:i\\(?:rectoryName\\|stanceToPoint\\)\\|ragAndDropTitle\\)\\|E\\(?:ditorCurve\\|xtension\\)\\|F\\(?:il\\(?:eName\\(?:WithoutExtension\\)?\\|tered\\)\\|loat\\(?:Value\\)?\\)\\|G\\(?:enericData\\|roundHit\\)\\|H\\(?:andleSize\\|eights\\)\\|I\\(?:n\\(?:s\\(?:pectorTitle\\|tanceID\\)\\|t\\(?:erpolatedNormal\\)?\\)\\|terator\\)\\|JoystickNames\\|Key\\(?:Down\\|Up\\)?\\|Last\\(?:Ping\\|Rect\\)\\|M\\(?:atrix\\|ouseButton\\(?:Down\\|Up\\)?\\)\\|NameOfFocusedControl\\|ObjectEnabled\\|P\\(?:ixel\\(?:Bilinear\\|s\\)?\\|o\\(?:int\\(?:Velocity\\)?\\|stprocessOrder\\)\\|refab\\(?:Parent\\|Type\\)\\)\\|R\\(?:e\\(?:ct\\|lativePointVelocity\\)\\|ow\\)\\|S\\(?:creenRect\\|ide\\|t\\(?:ateObject\\|r\\(?:eamProgressForLevel\\|ing\\)\\|yle\\)\\)\\|T\\(?:ag\\|e\\(?:mporary\\|xture\\(?:Offset\\|Scale\\)?\\)\\|r\\(?:\\(?:ansform\\|iangle\\)s\\)\\|ypeForControl\\)\\|UniqueTempPathInProject\\|Vector\\|Window\\(?:WithRect\\)?\\)\\)\\|izmo\\(?:Type\\(?:\\.\\(?:Active\\|NotSelected\\|Pickable\\|Selected\\(?:OrChild\\)?\\)\\)?\\|s\\)\\|raphics\\)\\|H\\(?:SVToRGB\\|a\\(?:ndle\\(?:Utility\\|s\\)\\|s\\(?:Character\\|HelpForObject\\|Key\\|ObjectThumbnail\\|Property\\|htable\\)\\|vePublicAddress\\)\\|e\\(?:ight\\|lp\\)\\|i\\(?:de\\(?:Flags\\(?:\\.\\(?:DontSave\\|Hide\\(?:AndDontSave\\|In\\(?:Hierarchy\\|Inspector\\)\\)\\|NotEditable\\)\\)?\\|InInspector\\)\\|\\(?:ngeJoin\\|tTes\\)t\\)\\|o\\(?:rizontalS\\(?:\\(?:crollba\\|lide\\)r\\)\\|stData\\)\\)\\|I\\(?:gnoreCollision\\|m\\(?:agePosition\\(?:\\.\\(?:Image\\(?:Above\\|Left\\|Only\\)\\|TextOnly\\)\\)?\\|portAsset\\(?:Options\\(?:\\.\\(?:Default\\|Force\\(?:SynchronousImport\\|Update\\)\\|ImportRecursive\\|TryFastReimportFromMetaData\\)\\)?\\)?\\)\\|n\\(?:AnimationMode\\|creaseLevel\\|finity\\|itializeSe\\(?:curity\\|rver\\)\\|put\\|s\\(?:pectorTitlebar\\|tan\\(?:ceIDToObject\\|tiate\\(?:Prefab\\)?\\)\\)\\|t\\(?:Field\\|Popup\\|Slider\\|ersectRay\\)\\|v\\(?:erse\\(?:Lerp\\|Transform\\(?:Direction\\|Point\\)\\)?\\|oke\\(?:Repeating\\)?\\)\\)\\|s\\(?:C\\(?:hildOf\\|reated\\)\\|Invoking\\|MainAsset\\|P\\(?:ersistent\\|laying\\)\\|Sleeping\\)\\)\\|Join\\(?:t\\(?:Drive\\(?:Mode\\(?:\\.\\(?:None\\|Position\\(?:AndVelocity\\)?\\|Velocity\\)\\)?\\)?\\|Limits\\|Motor\\|ProjectionMode\\(?:\\.\\(?:None\\|Position\\(?:AndRotation\\|Only\\)\\)\\)?\\|Spring\\)?\\)?\\|Key\\(?:Code\\(?:\\.\\(?:A\\(?:l\\(?:pha[0-9]\\|tGr\\)\\|mpersand\\|sterisk\\|t\\)\\|B\\(?:ack\\(?:Quote\\|s\\(?:lash\\|pace\\)\\)\\|reak\\)\\|C\\(?:a\\(?:psLock\\|ret\\)\\|lear\\|o\\(?:lon\\|mma\\)\\)\\|D\\(?:elete\\|o\\(?:llar\\|ubleQuote\\|wnArrow\\)\\)\\|E\\(?:nd\\|quals\\|scape\\|xclaim\\)\\|F\\(?:1[0-5]\\|[1-9]\\)\\|Greater\\|H\\(?:ash\\|elp\\|ome\\)\\|Insert\\|Joystick\\(?:1Button\\(?:1[0-9]\\|[0-9]\\)\\|2Button\\(?:1[0-9]\\|[0-9]\\)\\|3Button\\(?:1[0-9]\\|[0-9]\\)\\|Button\\(?:1[0-9]\\|[0-9]\\)\\)\\|Keypad\\(?:Divide\\|E\\(?:nter\\|quals\\)\\|M\\(?:inus\\|ultiply\\)\\|P\\(?:eriod\\|lus\\)\\|[0-9]\\)\\|Le\\(?:ft\\(?:A\\(?:lt\\|pple\\|rrow\\)\\|Bracket\\|Control\\|Paren\\|Shift\\|Windows\\)\\|ss\\)\\|M\\(?:inus\\|ouse[0-6]\\)\\|N\\(?:one\\|umlock\\)\\|P\\(?:a\\(?:ge\\(?:Down\\|Up\\)\\|use\\)\\|eriod\\|lus\\|rint\\)\\|Qu\\(?:estion\\|ote\\)\\|R\\(?:eturn\\|ight\\(?:A\\(?:lt\\|pple\\|rrow\\)\\|Bracket\\|Control\\|Paren\\|Shift\\|Windows\\)\\)\\|S\\(?:crollLock\\|emicolon\\|lash\\|pace\\|ysReq\\)\\|Tab\\|U\\(?:nderscore\\|pArrow\\)\\|[A-Z]\\)\\)?\\|boardEvent\\|frame\\)\\|L\\(?:INES\\|a\\(?:bel\\(?:Field\\)?\\|teUpdate\\|yer\\(?:Field\\|Mask\\|ToName\\)\\)\\|e\\(?:n\\(?:gth\\|sFlare\\)\\|rp\\(?:Angle\\)?\\)\\|i\\(?:ght\\(?:RenderMode\\(?:\\.\\(?:Auto\\|Force\\(?:Pixel\\|Vertex\\)\\)\\)?\\|Shadows\\(?:\\.\\(?:Hard\\|None\\|Soft\\)\\)?\\|Type\\(?:\\.\\(?:Directional\\|\\(?:Poin\\|Spo\\)t\\)\\)?\\|map\\(?:Data\\|Settings\\)\\)?\\|ne\\(?:Renderer\\|ar\\|cast\\)\\)\\|o\\(?:ad\\(?:A\\(?:ll\\(?:AssetsAtPath\\)?\\|s\\(?:setAtPath\\|ync\\)\\)\\|I\\(?:dentity\\|mage\\(?:IntoTexture\\)?\\)\\|Level\\(?:A\\(?:dditive\\(?:Async\\)?\\|sync\\)\\)?\\|MainAssetAtPath\\|Ortho\\|P\\(?:\\(?:ixel\\|rojection\\)Matrix\\)\\|Required\\|UnityWeb\\)?\\|ckReloadAssemblies\\|g\\(?:10\\|Callback\\|Error\\|Type\\(?:\\.\\(?:Assert\\|E\\(?:rror\\|xception\\)\\|\\(?:Lo\\|Warnin\\)g\\)\\)?\\|Warning\\)?\\|ok\\(?:At\\|Like\\(?:Controls\\|Inspector\\)\\|Rotation\\)\\)\\)\\|M\\(?:a\\(?:cros\\|sterServer\\(?:Event\\(?:\\.\\(?:HostListReceived\\|Registration\\(?:Failed\\(?:Game\\(?:\\(?:Nam\\|Typ\\)e\\)\\|NoServer\\)\\|Succeeded\\)\\)\\)?\\)?\\|t\\(?:erial\\(?:PropertyBlock\\)?\\|hf\\|rix4x4\\)\\|x\\(?:Height\\|Width\\)?\\)\\|e\\(?:nu\\(?:Command\\|Item\\)\\|sh\\(?:\\(?:Collid\\|Filt\\|Render\\)er\\)?\\)\\|in\\(?:Height\\|MaxRect\\|Width\\)?\\|o\\(?:delImporter\\(?:AnimationCompression\\(?:\\.\\(?:KeyframeReduction\\(?:AndCompression\\)?\\|Off\\)\\)?\\|ClipAnimation\\|Generate\\(?:Animations\\(?:\\.\\(?:In\\(?:Nodes\\|OriginalRoots\\|Root\\)\\|None\\)\\)?\\|Materials\\(?:\\.\\(?:None\\|Per\\(?:SourceMaterial\\|Texture\\)\\)\\)?\\)\\|MeshCompression\\(?:\\.\\(?:High\\|Low\\|Medium\\|Off\\)\\)?\\|TangentSpaceMode\\(?:\\.\\(?:All\\|None\\|OnlyNormals\\)\\)?\\)?\\|no\\(?:Behaviour\\|Script\\)\\|useCursor\\(?:\\.\\(?:Arrow\\|Link\\|MoveArrow\\|R\\(?:esize\\(?:Horizontal\\|Up\\(?:\\(?:Lef\\|Righ\\)t\\)\\|Vertical\\)\\|otateArrow\\)\\|S\\(?:\\(?:cal\\|lid\\)eArrow\\)\\|Text\\)\\)?\\|v\\(?:e\\(?:Asset\\(?:ToTrash\\)?\\|FileOrDirectory\\|Key\\|\\(?:Posi\\|Rota\\)tion\\)?\\|ie\\(?:Importer\\|Texture\\)\\)\\)\\|ult\\(?:Matrix\\|i\\(?:TexCoord[23]?\\|ply\\(?:Point\\(?:3x4\\)?\\|Vector\\)\\)\\)\\)\\|N\\(?:ameToLayer\\|e\\(?:gativeInfinity\\|twork\\(?:ConnectionError\\(?:\\.\\(?:AlreadyConnectedToAnotherServer\\|C\\(?:onnection\\(?:\\(?:Bann\\|Fail\\)ed\\)\\|reateSocketOrThreadFailure\\)\\|EmptyConnectTarget\\|In\\(?:correctParameters\\|\\(?:ternalDirectConnectFaile\\|validPasswor\\)d\\)\\|N\\(?:ATTarget\\(?:ConnectionLost\\|NotConnected\\)\\|oError\\)\\|RSAPublicKeyMismatch\\|TooManyConnectedPlayers\\)\\)?\\|Disconnection\\(?:\\.\\(?:Disconnected\\|LostConnection\\)\\)?\\|MessageInfo\\|P\\(?:eerType\\(?:\\.\\(?:C\\(?:lient\\|onnecting\\)\\|Disconnected\\|Server\\)\\)?\\|layer\\)\\|StateSynchronization\\(?:\\.\\(?:Off\\|ReliableDeltaCompressed\\|Unreliable\\)\\)?\\|View\\(?:ID\\)?\\)?\\|wScene\\|xt\\(?:Visible\\)?\\)\\|icifyVariableName\\|o\\(?:nSerialized\\|rmalize\\)\\)\\|O\\(?:bject\\(?:Content\\|Field\\|Names\\)?\\|n\\(?:A\\(?:pplication\\(?:Pause\\|Quit\\)\\|ssignMaterialModel\\)\\|Became\\(?:\\(?:Inv\\|V\\)isible\\)\\|Co\\(?:llision\\(?:E\\(?:nter\\|xit\\)\\|Stay\\)\\|n\\(?:nectedToServer\\|trollerColliderHit\\)\\)\\|D\\(?:estroy\\|is\\(?:able\\|connectedFromServer\\)\\|rawGizmos\\(?:Selected\\)?\\)\\|Enable\\|F\\(?:ailedToConnect\\(?:ToMasterServer\\)?\\|ocus\\)\\|GUI\\|HierarchyChange\\|Inspector\\(?:GUI\\|Update\\)\\|JointBreak\\|L\\(?:evelWasLoaded\\|ostFocus\\)\\|M\\(?:asterServerEvent\\|ouse\\(?:D\\(?:own\\|rag\\)\\|E\\(?:nter\\|xit\\)\\|Over\\|Up\\)\\)\\|NetworkInstantiate\\|P\\(?:articleCollision\\|layer\\(?:\\(?:C\\|Disc\\)onnected\\)\\|ost\\(?:Render\\|process\\(?:A\\(?:llAssets\\|udio\\)\\|GameObjectWithUserProperties\\|Model\\|Texture\\)\\)\\|r\\(?:e\\(?:Cull\\|Render\\|process\\(?:Audio\\|Model\\|Texture\\)\\)\\|ojectChange\\)\\)\\|Render\\(?:Image\\|Object\\)\\|S\\(?:ceneGUI\\|e\\(?:lectionChange\\|r\\(?:ializeNetworkView\\|verInitialized\\)\\)\\)\\|Trigger\\(?:E\\(?:nter\\|xit\\)\\|Stay\\)\\|Wi\\(?:llRenderObject\\|zard\\(?:Create\\|OtherButton\\|Update\\)\\)\\)\\|p\\(?:en\\(?:Asset\\|FilePanel\\|Project\\|Scene\\(?:Additive\\)?\\|URL\\)\\|timize\\)\\|rtho\\(?:Normalize\\)?\\|verlapSphere\\)\\|P\\(?:I\\|a\\(?:ckTextures\\|rticle\\(?:Animator\\|Emitter\\|Render\\(?:Mode\\(?:\\.\\(?:Billboard\\|HorizontalBillboard\\|S\\(?:ortedBillboard\\|tretch\\)\\|VerticalBillboard\\)\\)?\\|er\\)\\)?\\|sswordField\\|th\\|use\\)\\|er\\(?:form\\(?:\\(?:Re\\|Un\\)do\\)\\|spective\\)\\|hysic\\(?:Material\\(?:Combine\\(?:\\.\\(?:Average\\|M\\(?:aximum\\|inimum\\|ultiply\\)\\)\\)?\\)?\\|s\\)\\|i\\(?:ckGameObject\\|ng\\(?:Object\\|Pong\\)?\\)\\|la\\(?:ne\\|y\\(?:ClipAtPoint\\|Mode\\(?:\\.Stop\\(?:All\\|SameLayer\\)\\)?\\|OneShot\\|Queued\\|erPrefs\\(?:Exception\\)?\\)?\\)\\|o\\(?:llHostList\\|p\\(?:AssetDependencies\\|Camera\\|Matrix\\|up\\)\\|sitionHandle\\|[pw]\\)\\|r\\(?:e\\(?:f\\(?:abType\\(?:\\.\\(?:Disconnected\\(?:\\(?:Model\\)?PrefabInstance\\)\\|M\\(?:issingPrefabInstance\\|odelPrefab\\(?:Instance\\)?\\)\\|None\\|Prefab\\(?:Instance\\)?\\)\\)?\\|ixLabel\\)\\|pareStartDrag\\)\\|imitiveType\\(?:\\.\\(?:C\\(?:apsule\\|ube\\|ylinder\\)\\|\\(?:Plan\\|Spher\\)e\\)\\)?\\|o\\(?:filer\\|ject\\(?:PointLine\\|or\\)?\\|perty\\(?:Field\\|ToID\\)\\)\\)\\|ush\\(?:AssetDependencies\\|Camera\\|Matrix\\)?\\)\\|Q\\(?:UADS\\|u\\(?:a\\(?:lity\\(?:Level\\(?:\\.\\(?:Beautiful\\|Fa\\(?:ntastic\\|st\\(?:est\\)?\\)\\|Good\\|Simple\\)\\)?\\|Settings\\)\\|ternion\\)\\|e\\(?:ryStateObject\\|ue\\(?:GameViewInputEvent\\|Mode\\(?:\\.\\(?:CompleteOthers\\|PlayNow\\)\\)?\\)\\)\\|it\\)\\)\\|R\\(?:GBToHSV\\|PC\\(?:Mode\\(?:\\.\\(?:All\\(?:Buffered\\)?\\|Others\\(?:Buffered\\)?\\|Server\\)\\)?\\)?\\|a\\(?:d2Deg\\|n\\(?:dom\\|ge\\)\\|y\\(?:Snap\\|cast\\(?:All\\|Collider\\|Hit\\)?\\)?\\)\\|e\\(?:adPixels\\|c\\(?:alculate\\(?:\\(?:Bound\\|Normal\\)s\\)\\|onnectToLastPrefab\\|t\\(?:Field\\|Offset\\|angleCap\\)?\\)\\|f\\(?:lect\\|resh\\(?:Delayed\\)?\\)\\|gister\\(?:Host\\|LogCallback\\|S\\(?:ceneUndo\\|napshot\\)\\|Undo\\)\\|lease\\(?:Temporary\\)?\\|move\\(?:At\\|Clip\\|Key\\|Notification\\|RPCs\\(?:InGroup\\)?\\)?\\|n\\(?:ameAsset\\|der\\(?:BeforeQueues\\|GameViewCameras\\|Settings\\|T\\(?:exture\\(?:Format\\(?:\\.\\(?:ARGB\\(?:32\\|Half\\)\\|Depth\\)\\)?\\)?\\|oCubemap\\)\\|\\(?:WithShad\\)?er\\)?\\)\\|p\\(?:aint\\|eat\\(?:Button\\)?\\|lacePrefab\\(?:Options\\(?:\\.\\(?:ConnectToPrefab\\|Default\\|ReplaceNameBased\\|UseLastUploadedPrefabRoot\\)\\)?\\)?\\)\\|qu\\(?:\\(?:estHostLis\\|ireComponen\\)t\\)\\|s\\(?:et\\(?:Aspect\\|GameObjectToPrefabState\\|InputAxes\\|ProjectionMatrix\\|ReplacementShader\\|ToPrefabState\\|WorldToCameraMatrix\\)?\\|ize\\|o\\(?:lution\\|urces\\)\\|toreSnapshot\\)\\|verse\\|wind\\)\\|igidbody\\(?:Interpolation\\(?:\\.\\(?:\\(?:Extrapolat\\|Interpolat\\|Non\\)e\\)\\)?\\)?\\|o\\(?:tat\\(?:e\\(?:Around\\(?:Pivot\\)?\\|Towards\\)?\\|ion\\(?:DriveMode\\(?:\\.\\(?:Slerp\\|XYAndZ\\)\\)?\\|Handle\\)\\)\\|und\\(?:ToInt\\)?\\)\\|untimePlatform\\(?:\\.\\(?:\\(?:OSX\\(?:DashboardPlaye\\|Edito\\|\\(?:Web\\)?Playe\\)\\|Windows\\(?:Edito\\|\\(?:Web\\)?Playe\\)\\)r\\)\\)?\\)\\|S\\(?:a\\(?:m\\(?:eSide\\|ple\\(?:Animation\\|Height\\)?\\)\\|ve\\(?:Assets\\|CurrentSceneIfUserWantsTo\\|F\\(?:ilePanel\\(?:InProject\\)?\\|olderPanel\\)\\|Scene\\)\\)\\|c\\(?:ale\\(?:AroundPivot\\|Handle\\|Mode\\(?:\\.S\\(?:cale\\(?:AndCrop\\|ToFit\\)\\|tretchToFill\\)\\)?\\|Slider\\|ValueHandle\\)?\\|r\\(?:een\\(?:PointToRay\\|To\\(?:\\(?:GUI\\|Viewport\\|World\\)Point\\)\\)?\\|iptable\\(?:Object\\|Wizard\\)\\|ollTo\\)\\)\\|e\\(?:lection\\(?:Grid\\|Mode\\(?:\\.\\(?:Assets\\|Deep\\(?:Assets\\)?\\|E\\(?:ditable\\|xcludePrefab\\)\\|TopLevel\\|Unfiltered\\)\\)?\\)?\\|nd\\(?:Event\\|Message\\(?:Options\\(?:\\.\\(?:\\(?:Dont\\)?RequireReceiver\\)\\)?\\|Upwards\\)?\\)\\|parator\\|rializ\\(?:able\\|e\\(?:Field\\|d\\(?:Object\\|Property\\(?:Type\\(?:\\.\\(?:ArraySize\\|Boolean\\)\\)?\\)?\\)\\)?\\)\\)\\)\\)\\>" . font-lock-variable-name-face)
    '("\\<\\(S\\(?:e\\(?:rializedPropertyType\\.\\(?:C\\(?:\\(?:haracte\\|olo\\)r\\)\\|Enum\\|Float\\|Integer\\|LayerMask\\|ObjectReference\\|Rect\\|String\\|Vector[23]\\)\\|t\\(?:A\\(?:ctiveRecursively\\|nimation\\(?:\\(?:Clip\\|Event\\)s\\)\\)\\|Bo\\(?:ol\\|rderColor\\)\\|C\\(?:amera\\|ol\\(?:ors?\\|umn\\)\\|urve\\)\\|D\\(?:\\(?:ensi\\|ir\\)ty\\)\\|EditorCurve\\|F\\(?:loat\\|romToRotation\\)\\|G\\(?:enericData\\|lobal\\(?:Color\\|Float\\|Matrix\\|ShaderProperty\\|Texture\\|Vector\\)\\)\\|Heights\\|Int\\|L\\(?:evelPrefix\\|ookRotation\\)\\|M\\(?:\\(?:atri\\|inMa\\)x\\)\\|N\\(?:ameSmart\\|e\\(?:ighbors\\|xtControlName\\)\\)\\|ObjectEnabled\\|P\\(?:ass\\|ixels?\\|osition\\)\\|R\\(?:e\\(?:ceivingEnabled\\|placementShader\\|solution\\|vertBackfacing\\)\\|ow\\)\\|S\\(?:cope\\|endingEnabled\\|napshotTarget\\|tring\\)\\|T\\(?:RS\\|exture\\(?:Offset\\|Scale\\)?\\|riangles\\)\\|Ve\\(?:ctor\\|rtexCount\\)\\|Width\\)\\)\\|h\\(?:ader\\|ift\\|ow\\(?:Help\\(?:ForObject\\|Page\\)\\|Notification\\|Utility\\)?\\)\\|i\\(?:gn\\|m\\(?:\\(?:pleMov\\|ulat\\)e\\)\\|n\\)\\|k\\(?:in\\(?:Quality\\(?:\\.\\(?:Auto\\|Bone[124]\\)\\)?\\|nedMeshRenderer\\)\\|ybox\\)\\|l\\(?:e\\(?:[er]p\\)\\|ider\\)\\|mooth\\(?:Damp\\(?:Angle\\)?\\|Step\\|Tangents\\)\\|o\\(?:\\(?:ftJointLimi\\|r\\)t\\)\\|p\\(?:ace\\(?:\\.\\(?:Self\\|World\\)\\)?\\|hereC\\(?:ap\\|ollider\\)\\|ringJoint\\)\\|qr\\(?:Distance\\|t\\)\\|t\\(?:art\\(?:A\\(?:nimationMode\\|ssetEditing\\)\\|Coroutine\\|Drag\\)?\\|ep\\|op\\(?:A\\(?:llCoroutines\\|nimationMode\\|ssetEditing\\)\\|Coroutine\\)?\\|ring\\)\\|upportsRenderTextureFormat\\|y\\(?:ncLayer\\|stem\\(?:Info\\|Language\\(?:\\.\\(?:A\\(?:frikaans\\|rabic\\)\\|B\\(?:asque\\|\\(?:elarus\\|ulgar\\)ian\\)\\|C\\(?:atalan\\|hinese\\|zech\\)\\|D\\(?:\\(?:anis\\|utc\\)h\\)\\|E\\(?:nglish\\|stonian\\)\\|F\\(?:aroese\\|\\(?:inni\\|ren\\)sh\\)\\|G\\(?:erman\\|reek\\)\\|H\\(?:ebrew\\|ugarian\\)\\|I\\(?:celandic\\|\\(?:ndones\\|tal\\)ian\\)\\|Japanese\\|Korean\\|L\\(?:\\(?:atv\\|ithuan\\)ian\\)\\|Norwegian\\|Po\\(?:lish\\|rtuguese\\)\\|R\\(?:\\(?:oman\\|uss\\)ian\\)\\|S\\(?:erboCroatian\\|lov\\(?:ak\\|enian\\)\\|\\(?:pan\\|wed\\)ish\\)\\|T\\(?:hai\\|urkish\\)\\|U\\(?:\\(?:krainia\\|nknow\\)n\\)\\|Vietnamese\\)\\)?\\)\\)\\)\\|T\\(?:R\\(?:IANGLE\\(?:S\\|_STRIP\\)\\|S\\)\\|a\\(?:gField\\|n\\)\\|e\\(?:rrain\\(?:Collider\\|Data\\|Lighting\\(?:\\.\\(?:Lightmap\\|Pixel\\|Vertex\\)\\)?\\)?\\|st\\(?:Connection\\(?:NAT\\)?\\|PlanesAABB\\)\\|x\\(?:Coord[23]?\\|t\\(?:A\\(?:lignment\\(?:\\.\\(?:Center\\|\\(?:Lef\\|Righ\\)t\\)\\)?\\|nchor\\(?:\\.\\(?:Lower\\(?:Center\\|\\(?:Lef\\|Righ\\)t\\)\\|Middle\\(?:Center\\|\\(?:Lef\\|Righ\\)t\\)\\|Upper\\(?:Center\\|\\(?:Lef\\|Righ\\)t\\)\\)\\)?\\|rea\\|sset\\)\\|Clipping\\(?:\\.\\(?:Clip\\|Overflow\\)\\)?\\|Field\\|Mesh\\|ure\\(?:2D\\|Format\\(?:\\.\\(?:A\\(?:RGB32\\|lpha8\\)\\|DXT[15]\\|PVRTC_RGB\\(?:A[24]\\|[24]\\)\\|RGB24\\)\\)?\\|Importer\\(?:Format\\(?:\\.\\(?:A\\(?:RGB\\(?:16\\|32\\)\\|lpha8\\|utomatic\\)\\|DXT[15]\\|PVRTC_RGB\\(?:A[24]\\|[24]\\)\\|RGB\\(?:16\\|24\\)\\)\\)?\\|GenerateCubemap\\(?:\\.\\(?:Cylindrical\\|N\\(?:iceSpheremap\\|one\\)\\|S\\(?:\\(?:impleS\\)?pheremap\\)\\)\\)?\\|MipFilter\\(?:\\.\\(?:\\(?:Box\\|Kaiser\\)Filter\\)\\)?\\|N\\(?:POTScale\\(?:\\.\\(?:None\\|To\\(?:Larger\\|Nearest\\|Smaller\\)\\)\\)?\\|ormalFilter\\(?:\\.S\\(?:obel\\|tandard\\)\\)?\\)\\)?\\|WrapMode\\(?:\\.\\(?:Clamp\\|Repeat\\)\\)?\\)?\\)\\)\\)\\|hreadPriority\\(?:\\.\\(?:BelowNormal\\|High\\|Low\\|Normal\\)\\)?\\|ime\\|o\\(?:AngleAxis\\|String\\|ggle\\|olbar\\)\\|r\\(?:a\\(?:ilRenderer\\|ns\\(?:form\\(?:Direction\\|Point\\)?\\|late\\)\\)\\|ueTypeFontImporter\\)\\)\\|U\\(?:n\\(?:EscapeURL\\|do\\|focusWindow\\|lo\\(?:ad\\(?:UnusedAssets\\)?\\|ckReloadAssemblies\\)\\|\\(?:registerHos\\|shif\\)t\\)\\|\\(?:pdat\\|s\\)e\\)\\|V\\(?:alidateMoveAsset\\|e\\(?:ctor\\(?:[234]Field\\|[234]\\)\\|rt\\(?:ex3?\\|icalS\\(?:\\(?:crollba\\|lide\\)r\\)\\)\\)\\|iewport\\(?:PointToRay\\|To\\(?:\\(?:Screen\\|World\\)Point\\)\\)?\\)\\|W\\(?:WW\\(?:Form\\)?\\|a\\(?:itFor\\(?:EndOfFrame\\|FixedUpdate\\|Seconds\\)\\|keUp\\)\\|heel\\(?:Collider\\|FrictionCurve\\|Hit\\)\\|i\\(?:dth\\|ndow\\)\\|orld\\(?:\\(?:PointToSizedRec\\|To\\(?:\\(?:GUI\\|Screen\\|Viewport\\)Poin\\)\\)t\\)\\|rapMode\\(?:\\.\\(?:ClampForever\\|Default\\|Loop\\|Once\\|PingPong\\)\\)?\\)\\|YieldInstruction\\|a\\(?:bsoluteURL\\|c\\(?:celeration\\|ti\\(?:onKey\\|ve\\(?:ControlID\\|GameObject\\|InstanceID\\|Object\\|T\\(?:errain\\|ransform\\)\\)?\\)\\)\\|ddCollider\\|l\\(?:ignment\\|lCameras\\|t\\)\\|mbientLight\\|n\\(?:chor\\|g\\(?:le\\|ular\\(?:Drag\\|Velocity\\|X\\(?:Drive\\|Motion\\)\\|Y\\(?:Limit\\|Motion\\|ZDrive\\)\\|Z\\(?:Limit\\|Motion\\)\\)\\)\\|i\\(?:mat\\(?:e\\(?:OnlyIfVisible\\|Physics\\)\\|ion\\(?:Compression\\|\\(?:Stat\\|WrapMod\\)e\\)?\\)\\|so\\(?:Level\\|tropicFiltering\\)\\)\\|yKey\\(?:Down\\)?\\)\\|pplication\\(?:\\(?:Contents\\)?Path\\)\\|s\\(?:pect\\(?:Ratio\\)?\\|set\\(?:Bundle\\|Importer\\|Path\\)?\\|ymptote\\(?:Slip\\|Value\\)\\)\\|tt\\(?:achedRigidbody\\|enuate\\)\\|u\\(?:dio\\(?:Clip\\)?\\|to\\(?:RepaintOnSceneChange\\|destruct\\)\\)\\|xis\\)\\|b\\(?:a\\(?:ckground\\(?:Color\\|LoadingPriority\\)?\\|keIK\\|\\(?:rycentricCoordinat\\|semapDistanc\\)e\\)\\|indposes\\|l\\(?:ack\\|\\(?:endMod\\|u\\)e\\)\\|o\\(?:ld\\(?:Font\\|Label\\)\\|ne\\(?:Index[0-3]\\|\\(?:Weight\\)?s\\)\\|olValue\\|rder\\(?:Mipmap\\)?\\|ttom\\|un\\(?:c\\(?:e\\(?:Combine\\|Threshold\\)\\|yness\\)\\|ds\\)\\|x\\)\\|r\\(?:akeTorque\\|eak\\(?:\\(?:Forc\\|Torqu\\)e\\)\\|ightness\\)\\|utton\\|ytes\\)\\|c\\(?:a\\(?:mera\\(?:ToWorldMatrix\\|VelocityScale\\)?\\|p\\(?:sLock\\|tureFramerate\\)\\|stShadows\\)\\|enter\\(?:OfMass\\)?\\|h\\(?:a\\(?:n\\(?:ged\\|nels\\)\\|racter\\)\\|ildCount\\)\\|l\\(?:ear\\(?:Flags\\)?\\|i\\(?:ckCount\\|p\\(?:Animations\\|ping\\)?\\)\\)\\|o\\(?:l\\(?:li\\(?:der\\|sionFlags\\)\\|or\\(?:Animation\\|Field\\|Value\\|s\\)?\\)\\|m\\(?:m\\(?:and\\(?:Name\\)?\\|ent\\)\\|pressionBitrate\\)\\|n\\(?:figuredInWorldSpace\\|nect\\(?:ed\\(?:Body\\|Players\\)\\|ion\\(?:Tester\\(?:IP\\|Port\\)\\|s\\)\\)\\|stantForce\\|t\\(?:acts\\|e\\(?:nt\\(?:Color\\|Offset\\)\\|xt\\)\\|rol\\(?:ler\\)?\\)\\|ve\\(?:rtToNormalmap\\|x\\)\\)\\|okie\\|rrectGamma\\)\\|u\\(?:llingMask\\|r\\(?:rent\\(?:Camera\\|Level\\|Resolution\\|Scene\\)?\\|sor\\(?:Color\\|FlashSpeed\\)\\|ve\\)\\|stomStyles\\)\\|yan\\)\\|d\\(?:a\\(?:mp\\(?:er\\|ing\\)\\|ta\\(?:Path\\)?\\)\\|e\\(?:compressOnLoad\\|dicatedServer\\|lta\\(?:Time\\)?\\|pth\\(?:TextureMode\\)?\\|t\\(?:ailObjectDistance\\|ectCollisions\\)\\)\\|i\\(?:rection\\|stance\\)\\|o\\(?:esAnimateColor\\|ubleClickSelectsWord\\)\\|rag\\|uration\\|ynamicFriction2?\\)\\|e\\(?:ditable\\|mit\\(?:terVelocityScale\\)?\\|n\\(?:abled\\|dWidth\\|ergy\\|um\\(?:Names\\|ValueIndex\\)\\)\\|rror\\(?:String\\)?\\|ulerAngles\\|xt\\(?:e\\(?:nts\\|rnal\\(?:IP\\|Port\\)\\)\\|remum\\(?:Slip\\|Value\\)\\)\\)\\|f\\(?:a\\(?:deout\\|rClipPlane\\)\\|i\\(?:eldOfView\\|lterMode\\|rstFrame\\|xed\\(?:DeltaTime\\|Height\\|Time\\|Width\\)\\)\\|l\\(?:are\\(?:Strength\\)?\\|oat\\(?:Parameter\\|Value\\)\\)\\|o\\(?:cused\\(?:Window\\)?\\|g\\(?:Color\\|Density\\)?\\|ldout\\(?:PreDrop\\)?\\|nt\\(?:\\(?:RenderMod\\|Siz\\|TextureCas\\)e\\)?\\|r\\(?:ce\\(?:ToMono\\)?\\|mat\\|ward\\(?:Dir\\|Friction\\|Slip\\)?\\)\\)\\|r\\(?:ame\\(?:Count\\|Rate\\)\\|e\\(?:e\\(?:\\(?:Spi\\|zeRotatio\\)n\\)\\|quency\\)\\|iction\\(?:Combine\\|Direction2\\)\\)\\|u\\(?:llScreen\\|nction\\(?:Key\\|Name\\)\\)\\)\\|g\\(?:ame\\(?:Name\\|Objects?\\|Type\\)\\|enerate\\(?:Animations\\|Cubemap\\|Materials\\)\\|lobal\\(?:MaximumLOD\\|Scale\\)\\|r\\(?:a\\(?:phics\\(?:Device\\(?:Name\\|Ve\\(?:ndor\\|rsion\\)\\)\\|MemorySize\\|ShaderLevel\\)\\|vity\\|y\\(?:scale\\(?:ToAlpha\\)?\\)?\\)\\|e\\(?:en\\|y\\)\\|oup\\)\\|uiText\\(?:ure\\)?\\)\\|h\\(?:a\\(?:loStrength\\|s\\(?:\\(?:Visible\\)?Children\\)\\)\\|e\\(?:aders\\|ight\\(?:map\\(?:Height\\|MaximumLOD\\|PixelError\\|Scale\\|Width\\)\\)?\\|lpString\\)\\|i\\(?:deFlags\\|\\(?:gh\\(?:\\(?:AngularX\\|Twist\\)Limi\\)\\|ngeJoin\\)t\\)\\|o\\(?:rizontal\\(?:S\\(?:crollbar\\(?:LeftButton\\|RightButton\\|Thumb\\)?\\|lider\\(?:Thumb\\)?\\)\\)?\\|tControl\\|ver\\)\\)\\|i\\(?:dentity\\|gnoreL\\(?:ayers\\|istenerVolume\\)\\|mage\\(?:Position\\)?\\|n\\(?:Tangent\\|comingPassword\\|dentLevel\\|ertiaTensor\\(?:Rotation\\)?\\|putString\\|s\\(?:ideUnit\\(?:\\(?:Circl\\|Spher\\)e\\)\\|tanceIDs\\)\\|t\\(?:Value\\|e\\(?:nsity\\|rpolation\\)\\)\\|verse\\)\\|p\\(?:Address\\)?\\|s\\(?:C\\(?:lient\\|ompiling\\|ubemap\\)\\|D\\(?:ebugBuild\\|one\\)\\|E\\(?:ditor\\|xpanded\\)\\|Grounded\\|I\\(?:dentity\\|nstantiatedPrefab\\)\\|K\\(?:ey\\|inematic\\)\\|LoadingLevel\\|M\\(?:essageQueueRunning\\|\\(?:in\\|ous\\)e\\)\\|P\\(?:aused\\|laying\\(?:OrWillChangePlaymode\\)?\\|owerOfTwo\\)\\|Read\\(?:able\\|ing\\|yToPlay\\)\\|S\\(?:erver\\|upported\\)\\|Trigger\\|V\\(?:alid\\|isible\\)\\|Writing\\)\\)\\|key\\(?:Code\\|boardControl\\|s\\)\\|l\\(?:a\\(?:bel\\|rgeLabel\\|stFrame\\|yer\\(?:CullDistances\\|MaskField\\)?\\)\\|e\\(?:ft\\|ngth\\(?:Scale\\)?\\|velCount\\)\\|i\\(?:ght\\(?:ing\\|map\\(?:Index\\|TilingOffset\\|s\\)?\\)?\\|mits?\\|ne\\(?:Height\\|Spacing\\|arLimit\\)\\)\\|o\\(?:adedLevel\\(?:Name\\)?\\|c\\(?:al\\(?:EulerAngles\\|Position\\|Rotation\\(?:Axis\\)?\\|Scale\\|ToWorldMatrix\\|Velocity\\)\\|kCursor\\)\\|g\\(?:File\\)?\\|op\\|ssyScale\\|w\\(?:\\(?:AngularX\\|Twist\\)Limit\\)\\)\\)\\|m\\(?:a\\(?:g\\(?:enta\\|nitude\\)\\|in\\(?:Asset\\|Texture\\(?:Offset\\|Scale\\)?\\)?\\|rgin\\|s\\(?:s\\|terTextureLimit\\)\\|t\\(?:erials?\\|rix\\)\\|x\\(?:AngularVelocity\\|Bounce\\|Connections\\|Distance\\|E\\(?:mission\\|nergy\\)\\|ParticleSize\\|QueuedFrames\\|Size\\|TextureSize\\|Volume\\|imum\\(?:Force\\|LOD\\)\\)?\\)\\|es\\(?:h\\(?:Compression\\)?\\|sageOptions\\)\\|i\\(?:n\\(?:Bounce\\|Distance\\|E\\(?:mission\\|nergy\\)\\|PenetrationForPenalty\\|Size\\|Volume\\|i\\(?:Button\\(?:Left\\|Mid\\|Right\\)?\\|Label\\|TextField\\|mumAllocatableViewIDs\\)\\)?\\|p\\(?:MapBias\\|map\\(?:Count\\|Enabled\\|F\\(?:adeDistance\\(?:End\\|Start\\)\\|ilter\\)\\)\\)\\)\\|o\\(?:d\\(?:e\\(?:lview\\)?\\|ifierKeysChanged\\)\\|tor\\(?:Torque\\)?\\|use\\(?:OverWindow\\|Position\\)\\|v\\(?:e\\(?:Direction\\|Length\\)\\|ie\\)\\)\\)\\|n\\(?:a\\(?:me\\|tFacilitator\\(?:IP\\|Port\\)\\)\\|e\\(?:ar\\(?:ClipPlane\\|estControl\\)\\|tworkView\\)\\|iceMouseDelta\\(?:Zoom\\)?\\|o\\(?:ne\\|rmal\\(?:SmoothingAngle\\|ized\\(?:Speed\\|Time\\)?\\|mapFilter\\|s\\)?\\|tification\\(?:Background\\|Text\\)\\)\\|potScale\\|um\\(?:berField\\|eric\\)\\)\\|o\\(?:b\\(?:ject\\(?:Field\\(?:Thumb\\)?\\|Reference\\(?:Parameter\\|Value\\|s\\)\\|s\\)\\|served\\)\\|ggVorbis\\|n\\(?:Active\\|Focused\\|Hover\\|Normal\\|\\(?:UnitSpher\\)?e\\)\\|peratingSystem\\|r\\(?:igin\\|thographic\\(?:Size\\)?\\)\\|therCollider\\|utTangent\\|verflow\\|wner\\)\\|p\\(?:a\\(?:dding\\|r\\(?:ent\\|ticle\\(?:Count\\|Emitter\\|RenderMode\\|s\\)\\)\\|ss\\(?:Count\\|wordProtected\\)\\|ths?\\|use\\)\\|eerType\\|i\\(?:tch\\|xel\\(?:Height\\|Inset\\|LightCount\\|Offset\\|Rect\\|Width\\)\\)\\|la\\(?:tform\\|y\\(?:Automatically\\|OnAwake\\|er\\(?:Limit\\)?\\|modeStateChanged\\)\\)\\|o\\(?:int\\|pup\\|rt\\|s\\(?:ition\\(?:Damper\\|Spring\\)?\\|tWrapMode\\)\\)\\|r\\(?:e\\(?:WrapMode\\|fabOverride\\|view\\)\\|iority\\|o\\(?:cessor\\(?:Count\\|Type\\)\\|gress\\|jection\\(?:Angle\\|Distance\\|M\\(?:atrix\\|ode\\)\\)\\|perty\\(?:Name\\|Path\\|Type\\)\\|xy\\(?:IP\\|P\\(?:assword\\|ort\\)\\)\\)\\)\\)\\|quality\\|r\\(?:a\\(?:dius\\|nge\\)\\|e\\(?:altimeSinceStartup\\|c\\(?:alculateNormals\\|eiveShadows\\|ommendedTextureFormat\\|t\\(?:Value\\)?\\)\\|d\\|freshRate\\|lative\\(?:Force\\|Torque\\|Velocity\\)\\|nder\\(?:Mode\\|Queue\\|er\\)\\|solutions\\)\\|ig\\(?:ht\\|idbody\\)\\|nd\\(?:AngularVelocity\\|Force\\|Rotation\\|Velocity\\)\\|o\\(?:lloffFactor\\|ot\\|tation\\(?:DriveMode\\)?\\)\\|pm\\|unInBackground\\)\\|s\\(?:amples\\|crollView\\|e\\(?:ed\\|lectionColor\\|nd\\(?:Rate\\|er\\)\\|rializedObject\\|ttings\\)\\|h\\(?:a\\(?:d\\(?:er\\|ow\\(?:C\\(?:\\(?:ascade\\|onstantBia\\)s\\)\\|Distance\\|ObjectSizeBias\\|Strength\\|s\\)\\)\\|redM\\(?:aterials?\\|esh\\)\\)\\|ift\\|owCursor\\)\\|i\\(?:deways\\(?:Dir\\|Friction\\|Slip\\)\\|ze\\(?:Grow\\)?\\)\\|k\\(?:in\\(?:Normals\\)?\\|ybox\\)\\|l\\(?:e\\(?:ep\\(?:\\(?:Angular\\)?Velocity\\)\\|rpDrive\\)\\|opeLimit\\)\\|mooth\\(?:DeltaTime\\|SphereCollisions\\)\\|o\\(?:ftVegetation\\|lverIterationCount\\)\\|p\\(?:eed\\|lit\\(?:\\(?:Animation\\|TangentsAcrossSeam\\)s\\)\\|otAngle\\|ring\\)\\|qrMagnitude\\|rcValue\\|t\\(?:a\\(?:ndardFont\\|rtWidth\\|t\\(?:eSynchronization\\|icFriction2?\\)\\)\\|e\\(?:erAngle\\|pOffset\\)\\|iffness\\|r\\(?:e\\(?:amedBytes\\|tch\\(?:Height\\|Width\\)\\)\\|ing\\(?:Parameter\\|Value\\)?\\|uctHeadingLabel\\)\\)\\|u\\(?:bMesh\\(?:Count\\|Index\\)\\|pports\\(?:\\(?:ImageEffect\\|RenderTexture\\|Shadow\\)s\\)\\|spension\\(?:Distance\\|Spring\\)\\)\\|w\\(?:apUVChannels\\|ing\\(?:1Limit\\|2Limit\\|Axis\\)\\)\\|ystem\\(?:CopyBuffer\\|\\(?:Languag\\|MemorySiz\\)e\\)\\)\\|t\\(?:a\\(?:bSize\\|g\\|ngent\\(?:SpaceMode\\|s\\)\\|rget\\(?:AngularVelocity\\|FrameRate\\|Object\\|Position\\|Rotation\\|Texture\\|Velocity\\)?\\)\\|e\\(?:rrainData\\|xt\\(?:Area\\|Color\\|Field\\|ure\\(?:Coord2?\\|Format\\)?\\)?\\)\\|h\\(?:isCollider\\|re\\(?:adPriority\\|eD\\)\\)\\|ime\\(?:S\\(?:amples\\|cale\\|ince\\(?:LevelLoad\\|Startup\\)\\)\\|stamp\\)?\\|o\\(?:ggle\\(?:Group\\)?\\|ol\\(?:bar\\(?:Button\\|DropDown\\|Popup\\|TextField\\)?\\|tip\\)\\|p\\|rque\\)\\|r\\(?:ans\\(?:forms?\\|pose\\)\\|ee\\(?:BillboardDistance\\|CrossFadeLength\\|Distance\\|MaximumFullLODCount\\)\\|i\\(?:angle\\(?:Index\\|s\\)\\|pleClickSelectsLine\\)\\)\\|ype\\)\\|u\\(?:n\\(?:assigned\\|ityVersion\\)\\|p\\(?:date\\(?:Rate\\|WhenOffscreen\\)?\\|loadProgress\\)\\|rl\\|se\\(?:ConeFriction\\|Gravity\\|Limits\\|M\\(?:ipMap\\|otor\\)\\|Nat\\|Proxy\\|Spring\\|WorldSpace\\|rData\\)\\|v\\(?:2\\|Animation\\(?:Cycles\\|[XY]Tile\\)\\)\\|[pv]\\)\\|v\\(?:alue\\|e\\(?:ctor\\(?:[23]Value\\)\\|locity\\(?:\\(?:Scal\\|UpdateMod\\)e\\)?\\|rt\\(?:exCount\\|ic\\(?:al\\(?:S\\(?:crollbar\\(?:DownButton\\|Thumb\\|UpButton\\)?\\|lider\\(?:Thumb\\)?\\)\\)?\\|es\\)\\)\\)\\|i\\(?:ewID\\|sualMode\\)\\|olume\\)\\|w\\(?:antsMouseMove\\|eight[0-3]?\\|hite\\(?:BoldLabel\\|La\\(?:\\(?:rgeLa\\)?bel\\)\\|MiniLabel\\|Texture\\)?\\|i\\(?:dth\\|ndow\\)\\|or\\(?:dWrap\\(?:pedLabel\\)?\\|ld\\(?:CenterOfMass\\|RotationAxis\\|To\\(?:\\(?:Camera\\|Local\\)Matrix\\)\\|Velocity\\)\\)\\|rapMode\\)\\|x\\(?:Drive\\|M\\(?:ax\\|\\(?:i\\|otio\\)n\\)\\)\\|y\\(?:Drive\\|M\\(?:ax\\|\\(?:i\\|otio\\)n\\)\\|ellow\\)\\|z\\(?:Drive\\|Motion\\|ero\\)\\|[abgrw-z]\\)\\>" . font-lock-variable-name-face)
    '("\\<\\(c\\(?:atch\\|lass\\)\\|else\\|f\\(?:or\\(?:each\\)?\\|unction\\)\\|i\\(?:f\\|mport\\)\\|new\\|p\\(?:rivate\\|ublic\\)\\|return\\|s\\(?:tatic\\|witch\\)\\|try\\|var\\|while\\)\\>" . font-lock-keyword-face))
   "Minimal highlighting expressions for unityjs mode")

(defconst unityjs-font-lock-keywords-2
  (append unityjs-font-lock-keywords-1
		  (list
		   '("\\<\\(Date\\|Number\\|Array\\|Object\\|Regex\\|b\\(oolean\\|yte\\)\\|char\\|double\\|float\\|int\\|long\\|s\\(byte\\|hort\\|tring\\)\\|u\\(long\\|\\(ni\\|shor\\)t\\)\\|void\\)\\>" . font-lock-type-face)
		   '("\\<\\(true\\|false\\|null\\)\\>" . font-lock-reference-face)))
  "Additional Keywords to highlight in UnityJS mode")

(defvar unityjs-font-lock-keywords unityjs-font-lock-keywords-2
  "Default highlighting expressions for UnityJS mode")


;;;;;;;;;;;;;;;;;
;; INDENTATION ;;
;;;;;;;;;;;;;;;;;

(defcustom js-indent-level 8
  "Number of spaces for each indentation step in `js-mode'."
  :type 'integer
  :group 'js)

(defcustom js-expr-indent-offset 0
  "Number of additional spaces used for indentation of continued expressions.
The value must be no less than minus `js-indent-level'."
  :type 'integer
  :group 'js)


(defun js--regexp-opt-symbol (list)
  "Like `regexp-opt', but surround the result with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" (regexp-opt list t) "\\_>"))


(defcustom js-comment-lineup-func #'c-lineup-C-comments
  "Lineup function for `cc-mode-style', for C comments in `js-mode'."
  :type 'function
  :group 'js)

(defun js--re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `js--re-search-backward'."
  (let ((parse)
        str-terminator
        (orig-macro-start
         (save-excursion
           (and (js--beginning-of-macro)
                (point)))))
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (forward-char))
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (save-excursion (beginning-of-line) (point)) t))
            ((nth 7 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            ((and (not (and orig-macro-start
                            (>= (point) orig-macro-start)))
                  (js--beginning-of-macro)))
            (t
             (setq count (1- count))))))
  (point))


(defconst js--opt-cpp-start "^\\s-*#\\s-*\\([[:alnum:]]+\\)"
  "Regexp matching the prefix of a cpp directive.
This includes the directive name, or nil in languages without
preprocessor support.  The first submatch surrounds the directive
name.")

(defun js--beginning-of-macro (&optional lim)
  (let ((here (point)))
    (save-restriction
      (if lim (narrow-to-region lim (point-max)))
      (beginning-of-line)
      (while (eq (char-before (1- (point))) ?\\)
        (forward-line -1))
      (back-to-indentation)
      (if (and (<= (point) here)
               (looking-at js--opt-cpp-start))
          t
        (goto-char here)
        nil))))

(defun js--re-search-backward (regexp &optional bound noerror count)
  "Search backward, ignoring strings, preprocessor macros, and comments.

This function invokes `re-search-backward' but treats the buffer
as if strings, preprocessor macros, and comments have been
removed.

If invoked while inside a macro, treat the macro as normal text."
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(js--re-search-backward-inner regexp bound 1))
               ((< count 0)
                '(js--re-search-forward-inner regexp bound (- count)))
               ((> count 0)
                '(js--re-search-backward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defconst js--possibly-braceless-keyword-re
  (js--regexp-opt-symbol
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with"
     "each"))
  "Regexp matching keywords optionally followed by an opening brace.")

(defconst js--indent-operator-re
  (concat "[-+*/%<>=&^|?:.]\\([^-+*/]\\|$\\)\\|"
          (js--regexp-opt-symbol '("in" "instanceof")))
  "Regexp matching operators that affect indentation of continued expressions.")


(defun js--looking-at-operator-p ()
  "Return non-nil if point is on a JavaScript operator, other than a comma."
  (save-match-data
    (and (looking-at js--indent-operator-re)
         (or (not (looking-at ":"))
             (save-excursion
               (and (js--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (looking-at "?")))))))


(defun js--continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (js--looking-at-operator-p)
        (and (js--re-search-backward "\n" nil t)
	     (progn
	       (skip-chars-backward " \t")
	       (or (bobp) (backward-char))
	       (and (> (point) (point-min))
                    (save-excursion (backward-char) (not (looking-at "[/*]/")))
                    (js--looking-at-operator-p)
		    (and (progn (backward-char)
				(not (looking-at "++\\|--\\|/[/*]"))))))))))


(defun js--end-of-do-while-loop-p ()
  "Return non-nil if point is on the \"while\" of a do-while statement.
Otherwise, return nil.  A braceless do-while statement spanning
several lines requires that the start of the loop is indented to
the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\_<while\\_>")
	(if (save-excursion
	      (skip-chars-backward "[ \t\n]*}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion
	      (backward-list) (forward-symbol -1) (looking-at "\\_<do\\_>"))
	  (js--re-search-backward "\\_<do\\_>" (point-at-bol) t)
	  (or (looking-at "\\_<do\\_>")
	      (let ((saved-indent (current-indentation)))
		(while (and (js--re-search-backward "^\\s-*\\_<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "\\s-*\\_<do\\_>")
		     (not (js--re-search-forward
			   "\\_<while\\_>" (point-at-eol) t))
		     (= (current-indentation) saved-indent)))))))))


(defun js--ctrl-statement-indentation ()
  "Helper function for `js--proper-indentation'.
Return the proper indentation of the current line if it starts
the body of a control statement without braces; otherwise, return
nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (point-at-bol) (point-min)))
                 (not (looking-at "[{]"))
                 (progn
                   (js--re-search-backward "[[:graph:]]" nil t)
                   (or (eobp) (forward-char))
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at js--possibly-braceless-keyword-re))
                 (not (js--end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) js-indent-level)))))

(defun js--get-c-offset (symbol anchor)
  (let ((c-offsets-alist
         (list (cons 'c js-comment-lineup-func))))
    (c-get-syntactic-indentation (list (cons symbol anchor)))))

(defun js--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 8 parse-status) 0) ; inside string
          ((js--ctrl-statement-indentation))
          ((eq (char-after) ?#) 0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ((nth 1 parse-status)
           (let ((same-indent-p (looking-at
                                 "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
                 (continued-expr-p (js--continued-expression-p)))
             (goto-char (nth 1 parse-status))
             (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                 (progn
                   (skip-syntax-backward " ")
		   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (cond (same-indent-p
                          (current-column))
                         (continued-expr-p
                          (+ (current-column) (* 2 js-indent-level)
                             js-expr-indent-offset))
                         (t
                          (+ (current-column) js-indent-level))))
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))

          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t 0))))

(defun js-indent-line ()
  "Indent the current line as JavaScript."
  (interactive)
  (save-restriction
    (widen)
    (let* ((parse-status
            (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation))))
      (indent-line-to (js--proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; INDENTATION ENDS  ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Syntax table
(defvar unityjs-mode-syntax-table
  (let ((unityjs-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" unityjs-mode-syntax-table)
    (modify-syntax-entry ?/ ". 124b" unityjs-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" unityjs-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" unityjs-mode-syntax-table)
    unityjs-mode-syntax-table)
  "Syntax table for unityjs-mode")


(defun unityjs-mode ()
  "Major mode for editing Unity Javascript files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table unityjs-mode-syntax-table)
  (use-local-map unityjs-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(unityjs-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'js-indent-line)
  (setq major-mode 'unityjs-mode)
  (setq mode-name "unityjs-mode")
  (run-hooks 'unityjs-mode-hook))

(provide 'unityjs-mode)
