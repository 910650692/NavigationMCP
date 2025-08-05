package com.sgm.navi.scene.impl.navi.common;

public class SceneCommonStruct {

    public enum QuickSearch {
        None(0),
        GasStation(1),
        ChargeStation(2),
        CarWashing(3),
        Food(4),
        Parking(5),
        Bathroom(6),
        Scenic(7),
        More(8),
        Home(9),
        Company(10),
        SetHome(11),
        SetCompany(12),
        Favorite(13);

        private final Object mValue;

        QuickSearch(final Object value) {
            this.mValue = value;
        }

        public static QuickSearch getDefault() {
            return None;
        }

        /**
         * @param value value
         * @return QuickSearch
         */
        public static QuickSearch get(final Object value) {
            for (QuickSearch obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum NearbyRecommend {
        RECOMMEND(0),
        FOOD(1),
        SCENIC(2),
        MALL(3),
        CHARGE_STATION(4),
        PARK(5),
        RESET_ROOM(6),
        SERVICE_AREA(7),
        OFFICE(8);

        private final Object mValue;

        NearbyRecommend(final Object value) {
            this.mValue = value;
        }

        public static NearbyRecommend getDefault() {
            return RECOMMEND;
        }

        /**
         * @param value value
         * @return NearbyRecommend
         */
        public static NearbyRecommend get(final Object value) {
            for (NearbyRecommend obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum SearchResultListIcon {
        SearchResultIconFirst(0),
        SearchResultIconSecond(1),
        SearchResultIconThird(2),
        SearchResultIconFourth(3),
        SearchResultIconFifth(4),
        SearchResultIconSixth(5),
        SearchResultIconSeventh(6),
        SearchResultIconEighth(7),
        SearchResultIconNinth(8),
        SearchResultIconTenth(9),
        SearchResultIconEleventh(10);

        private final Object mValue;

        SearchResultListIcon(final Object value) {
            this.mValue = value;
        }

        public int getItemValue() {
            return (int) mValue + 1;
        }

        public static SearchResultListIcon getDefault() {
            return SearchResultIconFirst;
        }

        /**
         * @param value value
         * @return SearchResultListIcon
         */
        public static SearchResultListIcon get(final Object value) {
            for (SearchResultListIcon obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum GusSearchHistoryIcon {
        UsusllyCardSearch(0), UsusllyCardLocation(1), OftenCardLocation(2);

        private final Object mValue;

        GusSearchHistoryIcon(final Object value) {
            this.mValue = value;
        }

        public static GusSearchHistoryIcon getDefault() {
            return UsusllyCardSearch;
        }

        /**
         * @param value value
         * @return GusSearchHistoryIcon
         */
        public static GusSearchHistoryIcon get(final Object value) {
            for (GusSearchHistoryIcon obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum EnumCommuteCard {
        CommuteCardGoHome(0),
        CommuteCardGoCompany(1),
        CommuteCardContinue(2),
        CommuteCardTeam(3),
        CommuteCardPOI(4),
        CommuteCardRoute(5);

        private final Object mValue;

        EnumCommuteCard(final Object value) {
            this.mValue = value;
        }

        public static EnumCommuteCard getDefault() {
            return CommuteCardGoHome;
        }

        /**
         * @param value value
         * @return EnumCommuteCard
         */
        public static EnumCommuteCard get(final Object value) {
            for (EnumCommuteCard obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum MapModeIcon {
        MAP_MODE_2D_NORTH(0),
        MAP_MODE_2D_HEAD(1),
        MAP_MODE_3D(2),
        MAP_ROAD_TRAFFIC_OPEN(3),
        MAP_ROAD_TRAFFIC_CLOSE(4),
        MAP_Broadcast_MUTE(5),
        MAP_Broadcast_MINIMALISM(6),
        MAP_Broadcast_EASY(7),
        MAP_Broadcast_DETAIL(8),
        MAP_LinkCar_TRUE(9),
        MAP_LinkCar_FALSE(10),
        MAP_FullScreen_EXPAND(11),
        MAP_FullScreen_CONTRACT(12),
        MAP_Setting(13);

        private final Object mValue;

        MapModeIcon(final Object value) {
            this.mValue = value;
        }

        public static MapModeIcon getDefault() {
            return MAP_MODE_2D_NORTH;
        }

        /**
         * @param value value
         * @return MapModeIcon
         */
        public static MapModeIcon get(final Object value) {
            for (MapModeIcon obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum SearchPoiLabel {
        LabelNone(0),
        LabelFavorite(1),
        LabelNavi(2),
        LabelView(3),
        LabelFastest(4),
        LabelClosest(5);

        private final Object mValue;

        SearchPoiLabel(final Object value) {
            this.mValue = value;
        }

        public static SearchPoiLabel getDefault() {
            return LabelNone;
        }

        /**
         * @param value value
         * @return SearchPoiLabel
         */
        public static SearchPoiLabel get(final Object value) {
            for (SearchPoiLabel obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum SearchListItemLabelType {
        LableTypeAmapGuide(0),
        LableTypeChargeHotTips(1),
        LableTypeParkingData(2),
        LableTypeFreeScenic(3),
        LableTypeProduct(4),
        LableTypeRecommend(5);

        private final Object mValue;

        SearchListItemLabelType(final Object value) {
            this.mValue = value;
        }

        public static SearchListItemLabelType getDefault() {
            return LableTypeAmapGuide;
        }

        /**
         * @param value value
         * @return SearchListItemLabelType
         */
        public static SearchListItemLabelType get(final Object value) {
            for (SearchListItemLabelType obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum DefaultIconType {
        DefaultIconTypeScenic(0),
        DefaultIconTypeLifeService(1),
        DefaultIconTypeUnknow(2),
        DefaultIconTypeHotel(3),
        DefaultIconTypeCarService(4),
        DefaultIconTypeFood(5);

        private final Object mValue;

        DefaultIconType(final Object value) {
            this.mValue = value;
        }

        public static DefaultIconType getDefault() {
            return DefaultIconTypeScenic;
        }

        /**
         * @param value value
         * @return DefaultIconType
         */
        public static DefaultIconType get(final Object value) {
            for (DefaultIconType obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum ExpresswayBrand {
        MaiDangLao(0),
        KFC(1),
        Starbuck(2),
        BurgeKing(3),
        Pizzahut(4),
        ZhenGongFu(5),
        YongHe(6),
        DeKeShi(7),
        Coast(8),
        SevenEleven(9),
        ZhouHeiya(10),
        DQ(11);

        private final Object mValue;

        ExpresswayBrand(final Object value) {
            this.mValue = value;
        }

        public static ExpresswayBrand getDefault() {
            return MaiDangLao;
        }

        /**
         * @param value value
         * @return ExpresswayBrand
         */
        public static ExpresswayBrand get(final Object value) {
            for (ExpresswayBrand obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum ExpresswayOil {
        ZhongShiYou(0),
        ZhongShiHua(1),
        QiaoPai(2),
        MeiFu(3),
        Unknow(4);

        private final Object mValue;

        ExpresswayOil(final Object value) {
            this.mValue = value;
        }

        public static ExpresswayOil getDefault() {
            return ZhongShiYou;
        }

        /**
         * @param value value
         * @return ExpresswayOil
         */
        public static ExpresswayOil get(final Object value) {
            for (ExpresswayOil obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum RouteWeatherType {
        WEATHER_SUNNY(100),
        WEATHER_CLOUDY(101),
        WEATHER_OVERCAST(104),
        WEATHER_FEW_CLOUDY(102),
        WEATHER_PARTLY_CLOUDY(103),
        WEATHER_WINDY(200),
        WEATHER_CALM(201),
        WEATHER_LIGHT_BREEZE(202),
        WEATHER_GENTLE_BREEZE(203),
        WEATHER_FRESH_BREEZE(204),
        WEATHER_STRONG_BREEZE(205),
        WEATHER_SHOWER_RAIN(300),
        WEATHER_HEAVY_SHOWER_RAIN(301),
        WEATHER_THUNDER_SHOWER(302),
        WEATHER_LIGHT_RAIN(305),
        WEATHER_DRIZZLE_RAIN(309),
        WEATHER_MODERATE_RAIN(306),
        WEATHER_HEAVY_RAIN(307),
        WEATHER_LIGHT_SNOW(400),
        WEATHER_MIST(500),
        WEATHER_SAND(503),
        WEATHER_DUST(504),
        WEATHER_HOT(900),
        WEATHER_COLD(901),
        WEATHER_ICE1(1001),
        WEATHER_ICE2(1002),
        WEATHER_GALE(207),
        WEATHER_STRONG_GALE(208),
        WEATHER_GALE_WARNING(5),
        WEATHER_TORNADO(212),
        WEATHER_WIND_STORM(209),
        WEATHER_VIOLENT_STORM(210),
        WEATHER_HURRICANE(211),
        WEATHER_TROPICAL_STROM(213),
        WEATHER_TYPHOON_WARNING(1),
        WEATHER_HEAVY_THUNDER_SHOWER(303),
        WEATHER_THUNDERSTORM_WARNING(17),
        WEATHER_THUNDER_WARNING(9),
        WEATHER_EXTREME_RAIN(308),
        WEATHER_STORM(310),
        WEATHER_HEAVY_STORM(311),
        WEATHER_SEVERE_STORM(312),
        WEATHER_STORM_WARNING(2),
        WEATHER_FREEZING_RAIN(313),
        WEATHER_HEAVY_SNOW(402),
        WEATHER_SNOW_STORM(403),
        WEATHER_BLIZZARD_WARNING(3),
        WEATHER_SLEET(404),
        WEATHER_RAIN_AND_SNOW(405),
        WEATHER_SHOWER_STORM(406),
        WEATHER_MODERATE_SNOW(401),
        WEATHER_SNOW_FLURRY(407),
        WEATHER_FOGGY(501),
        WEATHER_FOGGY_WARNING(12),
        WEATHER_HAZE_WARNING(13),
        WEATHER_DUSTHAZE_WARNING(16),
        WEATHER_DUSTSTORM(507),
        WEATHER_SANDSTORM(508),
        WEATHER_DUSTSTORM_WARNING(6),
        WEATHER_HAIL(304),
        WEATHER_HAIL_STONE(1003),
        WEATHER_HALE_WARNING(10),
        WEATHER_THUNDER_STORM(1004),
        WEATHER_THUNDER(1005),
        WEATHER_ICY_ROAD_WARNING(14),
        WEATHER_ROAD_SNOW_ICE_WARNING(21),
        WEATHER_FROST_WARNING(11),
        WEATHER_HIGH_WIND(206),
        WEATHER_HAZE(502),
        WEATHER_CONTINUOUS_HIGH_TEMPERATURE_WARNING(25),
        WEATHER_CONTINUOUS_LOW_TEMPERATURE_WARNING(24),
        WEATHER_COLD_WAVE_WARNING(4),
        WEATHER_HIGH_TEMPERATURE_WARNING(7),
        WEATHER_DROUGHT_WARNING(8),
        WEATHER_COLD_WARING(15),
        WEATHER_FOREST_FIRE_WARNING(18),
        WEATHER_COOLING_WARNING(19),
        WEATHER_LOW_TEMPERATURE_WARNING(20),
        WEATHER_HEAVY_AIR_POLLUTION_WARNING(22),
        WEATHER_DRY_HOT_WIND_WARNING(23),
        WEATHER_FIRE_WANING(26),
        WEATHER_DEFAULT(0);

        private final Object mValue;

        RouteWeatherType(final Object value) {
            this.mValue = value;
        }

        public static RouteWeatherType getDefault() {
            return WEATHER_SUNNY;
        }

        /**
         * @param value value
         * @return RouteWeatherType
         */
        public static RouteWeatherType get(final Object value) {
            for (RouteWeatherType obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum DefaultIconRectangleShapeType {
        DefaultIconTypeScenic(0),
        DefaultIconTypeLifeService(1),
        DefaultIconTypeUnknow(2),
        DefaultIconTypeHotel(3),
        DefaultIconTypeCarService(4),
        DefaultIconTypeFood(5),
        DefaultIconTypeShopping(6);

        private final Object mValue;

        DefaultIconRectangleShapeType(final Object value) {
            this.mValue = value;
        }

        public static DefaultIconRectangleShapeType getDefault() {
            return DefaultIconTypeScenic;
        }

        /**
         * @param value value
         * @return DefaultIconRectangleShapeType
         */
        public static DefaultIconRectangleShapeType get(final Object value) {
            for (DefaultIconRectangleShapeType obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum DestinationGuidePicture {
        FOOD(0),
        FOOD_1(1),
        FOOD_2(2),
        SCENIC(3),
        SCENIC_1(4),
        SCENIC_2(5),
        SHOPPING(6),
        SHOPPING_1(7),
        SHOPPING_2(8),
        PARKING(9),
        CAR_WASHING(10),
        GAS_STATION(11),
        CHARGE_STATION(12);

        private final Object mValue;

        DestinationGuidePicture(final Object value) {
            this.mValue = value;
        }

        public static DestinationGuidePicture getDefault() {
            return FOOD;
        }

        /**
         * @param value value
         * @return DestinationGuidePicture
         */
        public static DestinationGuidePicture get(final Object value) {
            for (DestinationGuidePicture obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum NaviButton {
        NaviBtnEnterPreview(0),
        NaviBtnExitPreview(1),
        NaviBtnExpandMore(2),
        NaviBtnCollapseMore(3),
        NaviBtnViaPoint(4),
        NaviBtnReport(5),
        NaviBtnCharging(6),
        NaviBtnSharing(7),
        NaviBtnTeam(8),
        NaviBtnRoadMain(9),
        NaviBtnRoadSide(10),
        NaviBtnBridgeUp(11),
        NaviBtnBridgeDown(12);

        private final Object mValue;

        NaviButton(final Object value) {
            this.mValue = value;
        }

        public static NaviButton getDefault() {
            return NaviBtnEnterPreview;
        }

        /**
         * @param value value
         * @return NaviButton
         */
        public static NaviButton get(final Object value) {
            for (NaviButton obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum AlongQuickSearch {
        None(0),
        GasStation(1),
        ChargeStation(2),
        Bathroom(3),
        Maintenance(4),
        Food(5);

        private final Object mValue;

        AlongQuickSearch(final Object value) {
            this.mValue = value;
        }

        public static AlongQuickSearch getDefault() {
            return None;
        }

        /**
         * @param value value
         * @return AlongQuickSearch
         */
        public static AlongQuickSearch get(final Object value) {
            for (AlongQuickSearch obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum LaneAction {
        LaneActionAheadAndNUll(0),
        LaneActionAheadAndNUllNoAction(1),
        LaneActionLeftAndNUll(2),
        LaneActionLeftAndNUllNoAction(3),
        LaneActionAheadLeftAndAhead(4),
        LaneActionAheadLeftAndLeft(5),
        LaneActionAheadLeftNoAction(6),
        LaneActionRightAndNUll(7),
        LaneActionRightAndNUllNoAction(8),
        LaneActionAheadRightAndAhead(9),
        LaneActionAheadRightAndRight(10),
        LaneActionAheadRightNoAction(11),
        LaneActionLUTurnAndNUll(12),
        LaneActionLUTurnAndNUllNoAction(13),
        LaneActionLeftRightAndLeft(14),
        LaneActionLeftRightAndRight(15),
        LaneActionLeftRightNoAction(16),
        LaneActionAheadLeftRightAndAhead(17),
        LaneActionAheadLeftRightAndLeft(18),
        LaneActionAheadLeftRightAndRight(19),
        LaneActionAheadLeftRightNoAction(20),
        LaneActionRUTurnAndNUll(21),
        LaneActionRUTurnAndNUllNoAction(22),
        LaneActionAheadLUTurnAndAhead(23),
        LaneActionAheadLUTurnAndLUTurn(24),
        LaneActionAheadLUTurnNoAction(25),
        LaneActionAheadRUTurnAndAhead(26),
        LaneActionAheadRUTurnAndRUTurn(27),
        LaneActionAheadRUTurnNoAction(28),
        LaneActionLeftLUTurnAndLeft(29),
        LaneActionLeftLUTurnAndLUTurn(30),
        LaneActionLeftLUTurnNoAction(31),
        LaneActionRightRUTurnAndRight(32),
        LaneActionRightRUTurnAndRUTurn(33),
        LaneActionRightRUTurnNoAction(34),
        LaneActionLeftInAheadAndNUll(35),
        LaneActionLeftInAheadAndNUllNoAction(36),
        LaneActionAheadLeftLUTurnAndAhead(37),
        LaneActionAheadLeftLUTurnAndLeft(38),
        LaneActionAheadLeftLUTurnAndLUTrun(39),
        fLaneActionAheadLeftLUTurnNoAction(40),
        LaneActionRightLUTurnAndRight(41),
        LaneActionRightLUTurnAndLUTurn(42),
        LaneActionRightLUTurnNoAction(43),
        LaneActionLeftRightLUTurnAndLeft(44),
        LaneActionLeftRightLUTurnAndRight(45),
        LaneActionLeftRightLUTurnAndLUTurn(46),
        LaneActionLeftRightLUTurnNoAction(47),
        LaneActionAheadRightLUTurnAndAhead(48),
        LaneActionAheadRightLUTurnAndRight(49),
        LaneActionAheadRightLUTurnAndLUTurn(50),
        LaneActionAheadRightLUTurnNoAction(51),
        LaneActionLeftRUTurnAndLeft(52),
        LaneActionLeftRUTurnAndRUTurn(53),
        LaneActionLeftRUTurnNoAction(54),
        LaneActionBusAndBus(55),
        LaneActionBusNoAction(56),
        LaneActionVariableAndVariable(57),
        LaneActionVariableNoAction(58),
        LaneActionDedicated(149),
        LaneActionTidal(150),
        LaneActionEmptyNoAction(151),
        LaneActionEmpty(152),
        LaneEtc(153),
        LaneLabor(154);

        private final Object mValue;

        LaneAction(final Object value) {
            this.mValue = value;
        }

        public static LaneAction getDefault() {
            return LaneActionAheadAndNUll;
        }

        /**
         * @param value value
         * @return enum type for value
         */
        public static LaneAction get(final Object value) {
            for (LaneAction obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum TollGateLaneAction {
        TollLaneTypeNULL(0x0),
        TollLaneTypeNormal(0x1),
        TollLaneTypeETC(0x2),
        TollLaneTypeAutomatric(0x4),
        TollLaneTypeAliPay(0x8),
        TollLaneTypeWechatPay(0x10),
        TollLaneTypeITC(0x20);

        private final Object mValue;

        TollGateLaneAction(final Object value) {
            this.mValue = value;
        }

        public static TollGateLaneAction getDefault() {
            return TollLaneTypeNULL;
        }

        /**
         * @param value value
         * @return enum type for value
         */
        public static TollGateLaneAction get(final Object value) {
            for (TollGateLaneAction obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum TimeLaneBottomAction {
        BackLaneBusWorkable(1),
        BackLaneSpecialWorkable(2),
        BackLaneTidalWorkable(3),
        BackLaneReversibleWorkable(4),
        BackLaneBusNoWorkable(5),
        BackLaneSpecialNoWorkable(6),
        BackLaneTidalNoWorkable(7),
        BackLaneReversibleNoWorkable(8);

        private final Object mValue;

        TimeLaneBottomAction(final Object value) {
            this.mValue = value;
        }

        public static TimeLaneBottomAction getDefault() {
            return BackLaneBusWorkable;
        }

        /**
         * @param value value
         * @return enum type for value
         */
        public static TimeLaneBottomAction get(final Object value) {
            for (TimeLaneBottomAction obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum NaviCardTrafficEvent {
        RoadForbid(0),
        TrafficIncident(1),
        RoadConstruction(2),
        RoadWater(3),
        Accidents(4),
        Congestion(5);

        private final Object mValue;

        NaviCardTrafficEvent(final Object value) {
            this.mValue = value;
        }

        public static NaviCardTrafficEvent getDefault() {
            return RoadForbid;
        }

        /**
         * @param value value
         * @return enum type for value
         */
        public static NaviCardTrafficEvent get(final Object value) {
            for (NaviCardTrafficEvent obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    /**
     * TODO：这边和TbtExitIconAction感觉没什么区别需要逻辑梳理
     */
    /*public enum TbtIconAction {
        ManeuverIconTurnLeft(2),
        ManeuverIconTurnRight(3),
        //轻微左转图标
        ManeuverIconSlightLeft(4),
        //轻微右转图标
        ManeuverIconSlightRight(5),
        ManeuverIconTurnHardLeft(6),
        ManeuverIconTurnHardRight(7),
        ManeuverIconUTurn(8),
        //继续执行图标
        ManeuverIconContinue(9),
        ManeuverIconWay(10),
        ManeuverIconEntryRing(11),
        ManeuverIconLeaveRing(12),
        ManeuverIconSAPA(13),
        ManeuverIconTollGate(14),
        ManeuverIconDestination(15),
        ManeuverIconTunnel(16),
        ManeuverIconEntryLeftRing(17),
        ManeuverIconLeaveLeftRing(18),
        ManeuverIconUTurnRight(19),
        ManeuverIconSpecialContinue(20),
        //进入环岛后左转图标
        ManeuverIconEntryRingLeft(21),
        //进入环岛后右转图标
        ManeuverIconEntryRingRight(22),
        //进入环岛后继续直行图标
        ManeuverIconEntryRingContinue(23),
        //进入环岛后掉头图标
        ManeuverIconEntryRingUTurn(24),
        ManeuverIconEntryLeftRingLeft(25),
        ManeuverIconEntryLeftRingRight(26),
        //进入左侧环岛后继续直行图标
        ManeuverIconEntryLeftRingContinue(27),
        //进入左侧环岛后掉头图标
        ManeuverIconEntryLeftRingUTurn(28),
        ManeuverIconEntryRing1(68),
        ManeuverIconEntryRing2(69),
        ManeuverIconEntryRing3(70),
        ManeuverIconEntryRing4(71),
        ManeuverIconEntryRing5(72),
        ManeuverIconEntryRing6(73),
        ManeuverIconEntryRing7(74),
        ManeuverIconEntryRing8(75),
        ManeuverIconEntryRing9(76),
        ManeuverIconEntryRing10(77),
        ManeuverIconLeaveRing1(79),
        ManeuverIconLeaveRing2(80),
        ManeuverIconLeaveRing3(81),
        ManeuverIconLeaveRing4(82),
        ManeuverIconLeaveRing5(83),
        ManeuverIconLeaveRing6(84),
        ManeuverIconLeaveRing7(85),
        ManeuverIconLeaveRing8(86),
        ManeuverIconLeaveRing9(87),
        ManeuverIconLeaveRing10(88),
        ManeuverIconMergeLeft(65),
        ManeuverIconMergeRight(66);

        private final Object mValue;

        TbtIconAction(final Object value) {
            this.mValue = value;
        }
    }*/

    public enum ViaPointType {
        Normal(0),
        ChargeStation(1),
        GasStation(2),
        EndPoint(3);

        private final Object mValue;

        ViaPointType(final Object value) {
            this.mValue = value;
        }

        public static ViaPointType getDefault() {
            return Normal;
        }

        /**
         * @param value value
         * @return ViaPointType
         */
        public static ViaPointType get(final Object value) {
            for (ViaPointType obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum TmcViaPointType {
        ViaPointType(0),
        ViaChargeType(1),
        ViaStarBucksType(2);

        private final Object mValue;

        TmcViaPointType(final Object value) {
            this.mValue = value;
        }

        public static TmcViaPointType getDefault() {
            return ViaPointType;
        }

        /**
         * @param value value
         * @return TmcViaPointType
         */
        public static TmcViaPointType get(final Object value) {
            for (TmcViaPointType obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum CarDirectionType {
        North(0),
        Northeast(1),
        East(2),
        Southeast(3),
        South(4),
        Southwest(5),
        West(6),
        Northwest(7);

        private final Object mValue;

        CarDirectionType(final Object value) {
            this.mValue = value;
        }

        public static CarDirectionType getDefault() {
            return North;
        }

        /**
         * @param value value
         * @return CarDirectionType
         */
        public static CarDirectionType get(final Object value) {
            for (CarDirectionType obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum TbtExitIconAction {
        ManeuverIconTurnLeft(2),
        ManeuverIconTurnRight(3),
        ManeuverIconSlightLeft(4),
        ManeuverIconSlightRight(5),
        ManeuverIconTurnHardLeft(6),
        ManeuverIconTurnHardRight(7),
        ManeuverIconUTurn(8),
        ManeuverIconContinue(9),
        ManeuverIconWay(10),
        ManeuverIconEntryRing(11),
        ManeuverIconLeaveRing(12),
        ManeuverIconSAPA(13),
        ManeuverIconTollGate(14),
        ManeuverIconDestination(15),
        ManeuverIconTunnel(16),
        ManeuverIconEntryLeftRing(17),
        ManeuverIconLeaveLeftRing(18),
        ManeuverIconUTurnRight(19),
        ManeuverIconSpecialContinue(20),
        fManeuverIconEntryRingLeft(21),
        ManeuverIconEntryRingRight(22),
        ManeuverIconEntryRingContinue(23),
        ManeuverIconEntryRingUTurn(24),
        //进入左侧环岛后左转图标
        ManeuverIconEntryLeftRingLeft(25),
        //进入左侧环岛后右转图标
        fManeuverIconEntryLeftRingRight(26),
        ManeuverIconEntryLeftRingContinue(27),
        ManeuverIconEntryLeftRingUTurn(28),
        ManeuverIconEntryRing1(68),
        ManeuverIconEntryRing2(69),
        ManeuverIconEntryRing3(70),
        ManeuverIconEntryRing4(71),
        ManeuverIconEntryRing5(72),
        ManeuverIconEntryRing6(73),
        ManeuverIconEntryRing7(74),
        ManeuverIconEntryRing8(75),
        ManeuverIconEntryRing9(76),
        ManeuverIconEntryRing10(77),
        ManeuverIconLeaveRing1(79),
        ManeuverIconLeaveRing2(80),
        ManeuverIconLeaveRing3(81),
        ManeuverIconLeaveRing4(82),
        ManeuverIconLeaveRing5(83),
        ManeuverIconLeaveRing6(84),
        ManeuverIconLeaveRing7(85),
        ManeuverIconLeaveRing8(86),
        ManeuverIconLeaveRing9(87),
        ManeuverIconLeaveRing10(88),
        ManeuverIconMergeLeft(65),
        ManeuverIconMergeRight(66);

        private final Object mValue;

        TbtExitIconAction(final Object value) {
            this.mValue = value;
        }

        public static TbtExitIconAction getDefault() {
            return ManeuverIconTurnLeft;
        }

        /**
         * @param value value
         * @return Enum
         */
        public static TbtExitIconAction get(final Object value) {
            for (TbtExitIconAction obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }

    public enum EnumSettingDraw {
        SettingDrawNaviLane(0),
        SettingDrawNaviNormal(1),
        SettingDrawRoadBar(2),
        SettingDrawNaviEagle(3);

        private final Object mValue;

        EnumSettingDraw(final Object value) {
            this.mValue = value;
        }

        public static EnumSettingDraw getDefault() {
            return SettingDrawNaviLane;
        }

        /**
         * @param value value
         * @return Enum
         */
        public static EnumSettingDraw get(final Object value) {
            for (EnumSettingDraw obj : values()) {
                if (obj.mValue.equals(value)) {
                    return obj;
                }
            }
            return getDefault();
        }
    }
}
