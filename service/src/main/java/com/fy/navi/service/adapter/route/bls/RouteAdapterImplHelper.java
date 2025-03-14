package com.fy.navi.service.adapter.route.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.DeviceUtils;
import com.android.utils.TimeUtils;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.common.model.Coord2DInt32;
import com.autonavi.gbl.common.path.model.GroupSegment;
import com.autonavi.gbl.common.path.option.LinkInfo;
import com.fy.navi.service.define.route.RouteL2Data;
import com.fy.navi.service.define.route.RoutePriorityType;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchParkInOutInfo;
import com.fy.navi.service.define.utils.BevPowerCarUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.aosclient.BLAosService;
import com.autonavi.gbl.aosclient.model.GReStrictedAreaResponseParam;
import com.autonavi.gbl.common.model.TbtCommonControl;
import com.autonavi.gbl.common.model.UserConfig;
import com.autonavi.gbl.common.model.WorkPath;
import com.autonavi.gbl.common.path.model.ChargeStationInfo;
import com.autonavi.gbl.common.path.model.EndPointEnergyInfo;
import com.autonavi.gbl.common.path.model.MainAction;
import com.autonavi.gbl.common.path.model.POIInfo;
import com.autonavi.gbl.common.path.model.PointType;
import com.autonavi.gbl.common.path.model.RestAreaInfo;
import com.autonavi.gbl.common.path.model.RestTollGateInfo;
import com.autonavi.gbl.common.path.model.RouteLimitInfo;
import com.autonavi.gbl.common.path.option.POIForRequest;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.common.path.option.RouteConstrainCode;
import com.autonavi.gbl.common.path.option.RouteOption;
import com.autonavi.gbl.common.path.option.RouteStrategy;
import com.autonavi.gbl.common.path.option.RouteType;
import com.autonavi.gbl.common.path.option.SegmentInfo;
import com.autonavi.gbl.common.path.option.UserAvoidInfo;
import com.autonavi.gbl.route.RouteService;
import com.autonavi.gbl.route.model.PathResultData;
import com.autonavi.gbl.route.model.RerouteParam;
import com.autonavi.gbl.route.model.RouteAlternativeChargeStationInfo;
import com.autonavi.gbl.route.model.RouteAlternativeChargeStationResult;
import com.autonavi.gbl.route.model.RouteCollisionSolution;
import com.autonavi.gbl.route.model.RouteInitParam;
import com.autonavi.gbl.route.model.RouteSerialParallelState;
import com.autonavi.gbl.route.model.WeatherLabelItem;
import com.autonavi.gbl.route.observer.IRouteAlternativeChargeStationObserver;
import com.autonavi.gbl.route.observer.IRouteResultObserver;
import com.autonavi.gbl.route.observer.IRouteWeatherObserver;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.route.RouteResultObserver;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteAlongCityInfo;
import com.fy.navi.service.define.route.RouteAlongCityParam;
import com.fy.navi.service.define.route.RouteAlterChargeStationInfo;
import com.fy.navi.service.define.route.RouteAlterChargeStationParam;
import com.fy.navi.service.define.route.RouteAvoidInfo;
import com.fy.navi.service.define.route.RouteChargeStationDetailInfo;
import com.fy.navi.service.define.route.RouteChargeStationInfo;
import com.fy.navi.service.define.route.RouteChargeStationParam;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.route.RouteLineSegmentInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.route.RoutePoint;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.aos.RestrictedAreaDetail;
import com.fy.navi.service.define.route.EvRangeOnRouteInfo;
import com.fy.navi.service.define.route.RouteRestAreaDetailsInfo;
import com.fy.navi.service.define.route.RouteRestAreaInfo;
import com.fy.navi.service.define.route.RouteRestAreaParam;
import com.fy.navi.service.define.route.RouteRestTollGateDetailsInfo;
import com.fy.navi.service.define.route.RouteRestTollGateInfo;
import com.fy.navi.service.define.route.RouteRestTollGateParam;
import com.fy.navi.service.define.route.RouteRestrictionInfo;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.define.route.RouteTrafficIncidentDetailsInfo;
import com.fy.navi.service.define.route.RouteTrafficIncidentInfo;
import com.fy.navi.service.define.route.RouteTrafficIncidentParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.route.RouteWeatherInfo;
import com.fy.navi.service.define.route.RouteWeatherParam;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

/**
 * RouteService辅助类.
 *
 * @Description Helper类只做对象及数据转换，不做原子能力调用
 * @Author lvww
 * @date 2024/12/5
 */
public class RouteAdapterImplHelper {
    private static final String TAG = MapDefaultFinalTag.ROUTE_SERVICE_TAG;
    private final RouteService mRouteService;
    /*** 该集合只存放算路结果回调 key = 哪个类请求的，value = requestResult，理论上集合的长度永远为1 **/
    private final Hashtable<String, RouteResultObserver> routeResultObserverHashtable;
    /*** 算路请求队列，会初始化一些的到结果前的一些参数 key = requestId，value = requestResult **/
    private Hashtable<Long, RequestRouteResult> routeResultDataHashtable;
    private long mRequsetId = -1;

    private int routeStrategy;
    private int routeConstrainCode;
    private UserAvoidInfo userAvoidInfo;
    private PoiInfoEntity mPoiInfoEntityEnd;

    protected RouteAdapterImplHelper(RouteService routeService, BLAosService blAosService) {
        routeResultObserverHashtable = new Hashtable<>();
        routeResultDataHashtable = new Hashtable<>();
        mRouteService = routeService;
        userAvoidInfo = new UserAvoidInfo();
    }

    protected void initRouteService() {
        initTbtComm();
        mRouteService.init(getRouteServiceParam());
        mRouteService.addRouteResultObserver(routeResultObserver);
        mRouteService.addRouteWeatherObserver(routeWeatherObserver);
    }

    private void initTbtComm() {
        String cache = GBLCacheFilePath.TBT_COMMON_CACHE_PATH;
        String navi = GBLCacheFilePath.OFFLINE_DOWNLOAD_DIR;
        WorkPath workPath = new WorkPath();
        workPath.cache = cache;
        workPath.navi = navi;
        UserConfig userConfig = new UserConfig();
        userConfig.deviceID = DeviceUtils.getDeviceId();
        userConfig.userBatch = "0";
        TbtCommonControl tbtCommonControl = TbtCommonControl.getInstance();
        tbtCommonControl.init(workPath, userConfig);
    }

    public Hashtable<Long, RequestRouteResult> getRouteResultDataHashtable() {
        if (ConvertUtils.isEmpty(routeResultDataHashtable))
            routeResultDataHashtable = new Hashtable<>();
        return routeResultDataHashtable;
    }

    protected RouteInitParam getRouteServiceParam() {
        RouteInitParam routeInitParam = new RouteInitParam();
        //所有的重算由HMI自行发起
        routeInitParam.rerouteParam = new RerouteParam(false, false);
        routeInitParam.collisionParam.state = RouteSerialParallelState.RouteSerial;
        routeInitParam.collisionParam.solution = RouteCollisionSolution.DiceCollision;
        routeInitParam.rerouteParam.enableAutoSwitchParallelReroute = true;
        return routeInitParam;
    }

    public void registerRouteObserver(String key, RouteResultObserver routeResultObserver) {
        routeResultObserverHashtable.put(key, routeResultObserver);
    }

    public void setRoutePreference(RoutePreferenceID routePreferenceID) {
        switch (routePreferenceID) {
            case PREFERENCE_RECOMMEND:
                routeStrategy = RouteStrategy.RouteStrategyPersonalGaodeBest;
                routeConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_AVOIDCONGESTION:
                routeStrategy = RouteStrategy.RouteStrategyPersonalTMC;
                routeConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_LESSCHARGE:
                routeStrategy = RouteStrategy.RouteStrategyPersonalLessMoney;
                routeConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_NOTHIGHWAY:
                routeStrategy = RouteStrategy.RouteStrategyPersonalLessHighway;
                routeConstrainCode = RouteConstrainCode.RouteAvoidFreeway | RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_FIRSTHIGHWAY:
                routeStrategy = RouteStrategy.RouteStrategyPersonalHighwayFirst;
                routeConstrainCode = RouteConstrainCode.RouteFreewayStrategy | RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_FIRSTMAINROAD:
                routeStrategy = RouteStrategy.RouteStrategyPersonalWidthFirst;
                routeConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_FASTESTSPEED:
                routeStrategy = RouteStrategy.RouteStrategyPersonalSpeedFirst;
                routeConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE:
                routeStrategy = RouteStrategy.RouteStrategyPersonalTMC2LessMoney;
                routeConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_NOTHIGHWAY:
                routeStrategy = RouteStrategy.RouteStrategyPersonalTMC2LessHighway;
                routeConstrainCode = RouteConstrainCode.RouteAvoidFreeway | RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTHIGHWAY:
                routeStrategy = RouteStrategy.RouteStrategyPersonalTMC2Highway;
                routeConstrainCode = RouteConstrainCode.RouteFreewayStrategy | RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_LESSCHARGE_AND_NOTHIGHWAY:
                routeStrategy = RouteStrategy.RouteStrategyPersonalLessMoney2LessHighway;
                routeConstrainCode = RouteConstrainCode.RouteAvoidFreeway | RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE_AND_NOTHIGHWAY:
                routeStrategy = RouteStrategy.RouteStrategyPersonalTMC2LessMondy2LessHighway;
                routeConstrainCode = RouteConstrainCode.RouteAvoidFreeway | RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTMAINROAD:
                routeStrategy = RouteStrategy.RouteStrategyPersonalTMC2WidthFirst;
                routeConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_FASTESTSPEED:
                routeStrategy = RouteStrategy.RouteStrategyPersonalTMC2SpeedFirst;
                routeConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
        }
    }

    public int getRouteType(int routePriorityType) {
        switch (routePriorityType) {
            case RoutePriorityType.ROUTE_TYPE_COMMON :
                return RouteType.RouteTypeCommon;
            case RoutePriorityType.ROUTE_TYPE_YAW:
                return RouteType.RouteTypeYaw;
            case RoutePriorityType.ROUTE_TYPE_CHANGE_STRATEGE:
                return RouteType.RouteTypeChangeStratege;
            case RoutePriorityType.ROUTE_TYPE_PARALLEL_ROAD:
                return RouteType.RouteTypeParallelRoad;
            case RoutePriorityType.ROUTE_TYPE_TMC:
                return RouteType.RouteTypeTMC;
            case RoutePriorityType.ROUTE_TYPE_LIMIT_LINE:
                return RouteType.RouteTypeLimitLine;
            case RoutePriorityType.ROUTE_TYPE_DAMAGED_ROAD:
                return RouteType.RouteTypeDamagedRoad;
            case RoutePriorityType.ROUTE_TYPE_CHANGE_JNY_PNT:
                return RouteType.RouteTypeChangeJnyPnt;
            case RoutePriorityType.ROUTE_TYPE_LIMIT_FORBID:
                return RouteType.RouteTypeLimitForbid;
            case RoutePriorityType.ROUTE_TYPE_MANUAL_REFRESH:
                return RouteType.RouteTypeManualRefresh;
            case RoutePriorityType.ROUTE_TYPE_LIMIT_FORBID_OFF_LINE:
                return RouteType.RouteTypeLimitForbidOffLine;
            case RoutePriorityType.ROUTE_TYPE_MUTI_ROUTE_REQUEST:
                return RouteType.RouteTypeMutiRouteRequest;
            case RoutePriorityType.ROUTE_TYPE_DISPATCH:
                return RouteType.RouteTypeDispatch;
            case RoutePriorityType.ROUTE_TYPE_VOICE_CHANGE_DEST:
                return RouteType.RouteTypeVoiceChangeDest;
            case RoutePriorityType.ROUTE_TYPE_GROUP_CHANGE_DEST:
                return RouteType.RouteTypeGroupChangeDest;
            case RoutePriorityType.ROUTE_TYPE_CHANGE_ROUTE_COVERTLY:
                return RouteType.RouteTypeChangeRouteCovertly;
        }
        return RouteType.RouteTypeCommon;
    }

    public void setAvoidRoad(RouteAvoidInfo routeAvoidInfo) {
        userAvoidInfo.linkList = routeAvoidInfo.getAvoidList();
    }

    public RouteOption getRequestParam(RequestRouteResult requestRouteResult, List<RouteParam> paramList) {
        RouteOption routeOption = new RouteOption();
        POIForRequest poiForRequest = convertParam2PoiForRequest(requestRouteResult, paramList);
        routeOption.setPOIForRequest(poiForRequest);//设置算路行程点
        if (requestRouteResult.isOnlineRoute()) {
            routeOption.setRouteStrategy(routeStrategy);
            routeOption.setRouteType(getRouteType(requestRouteResult.getRouteType()));//算路类型
            routeOption.setUserAvoidInfo(userAvoidInfo);
            int constrainCode = routeConstrainCode;
            if (BevPowerCarUtils.getInstance().isElecPlanRoute) {
                // 1.打开电动车接续算路（主路线上有充电桩）
                constrainCode |= RouteConstrainCode.RouteElecContinue;
                // 2.打开电动车接续多备选算路（所有路线上有充电桩）
                constrainCode |= RouteConstrainCode.RouteMultiContinueCalc;
                routeOption.setConstrainCode(constrainCode);
            }
            routeOption.setVehicleCharge(BevPowerCarUtils.getInstance().initlialHVBattenergy);
            userAvoidInfo.linkList.clear();
        } else {
            routeOption.setRouteType(getRouteType(requestRouteResult.getRouteType()));//算路类型
            routeOption.setRouteStrategy(routeStrategy);
            routeOption.setConstrainCode(routeConstrainCode | RouteConstrainCode.RouteCalcLocal);
            routeOption.setUserAvoidInfo(userAvoidInfo);
            routeOption.setOfflineReqCustomIdentityId("offline_request_id");
            routeOption.setVehicleCharge(BevPowerCarUtils.getInstance().initlialHVBattenergy);
            userAvoidInfo.linkList.clear();
        }
        return routeOption;
    }

    public void removeRouteObserver(String key) {
        routeResultObserverHashtable.remove(key);
    }

    public void removeAllObserver() {
        routeResultObserverHashtable.clear();
    }

    /**
     * 校验算路服务状态.
     */
    protected void checkoutRouteServer() {
        if (ServiceInitStatus.ServiceNotInit == mRouteService.isInit()) {
            initRouteService();
        }
    }

    /**
     * 转换请求参数，请求结果和算路请求都需要利用请求参数转换
     *
     * @param requestRouteResult 路线请求结果对象
     * @param paramList          路线请求参数
     * @return SDK需要的请求参数
     */
    private POIForRequest convertParam2PoiForRequest(RequestRouteResult requestRouteResult, List<RouteParam> paramList) {
        POIForRequest poiForRequest = new POIForRequest();
        for (RouteParam routeParam : paramList) {
            POIInfo poiInfo = GsonUtils.convertToT(routeParam, POIInfo.class);
            Logger.i(TAG, "poiInfo -> ", poiInfo);
            RouteLineLayerParam routeLineLayerParam = requestRouteResult.getLineLayerParam();
            switch (routeParam.getPoiType()) {
                case RoutePoiType.ROUTE_POI_TYPE_START -> {
                    RoutePoint routePoint = new RoutePoint();
                    routePoint.setPos(routeParam.getRealPos());
                    routePoint.setType(0);
                    routeLineLayerParam.getRouteLinePoints().getStartPoints().add(routePoint);
                    poiForRequest.addPoint(PointType.PointTypeStart, poiInfo);
                }
                case RoutePoiType.ROUTE_POI_TYPE_WAY -> {
                    RoutePoint routePoint = new RoutePoint();
                    routePoint.setPos(routeParam.getRealPos());
                    routePoint.setType(2);
                    routeLineLayerParam.getRouteLinePoints().getViaPoints().add(routePoint);
                    poiForRequest.addPoint(PointType.PointTypeVia, poiInfo);
                }
                case RoutePoiType.ROUTE_POI_TYPE_END -> {
                    RoutePoint routePoint = new RoutePoint();
                    routePoint.setPos(routeParam.getRealPos());
                    routePoint.setType(1);
                    routeLineLayerParam.getRouteLinePoints().getEndPoints().add(routePoint);
                    poiForRequest.addPoint(PointType.PointTypeEnd, poiInfo);
                }
                default -> Logger.i(TAG, "Poi点类型错误");
            }
        }
        return poiForRequest;
    }

    private List<RouteLineInfo> convertPathInfo2RouteResult(ArrayList<PathInfo> pathInfoList, boolean onlineRoute) {
        return getRouteLineInfoList(pathInfoList, onlineRoute);
    }

    /**
     * 路线基本信息
     **/
    private List<RouteLineInfo> getRouteLineInfoList(ArrayList<PathInfo> pathInfoList, boolean onlineRoute) {
        List<RouteLineInfo> routeResults = new ArrayList<>();
        if (!ConvertUtils.isEmpty(pathInfoList)) {
            for (PathInfo info : pathInfoList) {
                RouteLineInfo routeResult = new RouteLineInfo();
                routeResult.setPathID(info.getPathID());
                routeResult.setType(info.getType());
                routeResult.setLength(TimeUtils.getInstance().getDistanceString(info.getLength()));
                routeResult.setDis(info.getLength());
                routeResult.setTravelTime(TimeUtils.getInstance().getTimeStr(info.getTravelTime()));
                routeResult.setStaticTravelTime(TimeUtils.getInstance().getTimeStr(info.getStaticTravelTime()));
                routeResult.setTollCost("" + info.getTollCost());
                routeResult.setTrafficLightCount(info.getTrafficLightCount() + "");
                routeResult.setNaviID(info.getNaviID());
                routeResult.setOnline(info.isOnline());
                String elecRouteLabel = "不可达";
                boolean canBeArrive = false;
                if (onlineRoute) {
                    ArrayList<Integer> list = info.getElecPathInfo().mEnergyConsume.vehiclechargeleft;
                    if (!list.isEmpty() && list.get(list.size() - 1) != -1) {
                        Integer elecNum = list.get(list.size() - 1);
                        int elec = (int) (elecNum / BevPowerCarUtils.getInstance().maxBattenergy / 1000);
                        elecRouteLabel = elec + "%可达";
                        canBeArrive = true;
                        routeResult.setRemainPercent(elec);
                    }
                } else {
                    int remain = (int) (BevPowerCarUtils.getInstance().initlialHVBattenergy * BevPowerCarUtils.getInstance().batterToDistance - info.getLength());
                    if (remain > 0) {
                        int elec = (int) (remain * 100 / BevPowerCarUtils.getInstance().batterToDistance / BevPowerCarUtils.getInstance().maxBattenergy);
                        elecRouteLabel = elec + "%可达";
                        canBeArrive = true;
                        routeResult.setRemainPercent(elec);
                    }
                }
                routeResult.setElecRouteLabel(elecRouteLabel);
                routeResult.setCanBeArrive(canBeArrive);
                String label = "默认";
                if (info.getLabelInfoCount() > NumberUtils.NUM_0 && !ConvertUtils.isEmpty(info.getLabelInfo((short) NumberUtils.NUM_0).content)) {
                    label = info.getLabelInfo((short) NumberUtils.NUM_0).content;
                } else if (info.getReverseLabelInfoCount() > NumberUtils.NUM_0 && !ConvertUtils.isEmpty(info.getReverseLabelInfo((short) NumberUtils.NUM_0).content)) {
                    label = info.getReverseLabelInfo((short) NumberUtils.NUM_0).content;
                }
                routeResult.setLabel(label);
                routeResult.setRouteLineSegmentInfos(getRouteLineDetail(info));
                routeResults.add(routeResult);
            }
        }
        return routeResults;
    }

    /**
     * 路线详情
     **/
    private List<RouteLineSegmentInfo> getRouteLineDetail(PathInfo info) {
        List<RouteLineSegmentInfo> routeLineSegmentInfoParentList = new ArrayList<>();
        long groupSegmentCount = info.getGroupSegmentCount();
        if (groupSegmentCount > NumberUtils.NUM_0) {
            for (int t = NumberUtils.NUM_0; t < groupSegmentCount; t++) {
                RouteLineSegmentInfo routeLineSegmentInfoParent = new RouteLineSegmentInfo();
                //聚合段 -- 道路名
                String parentRoadName = "无名路";
                String roadName = info.getGroupSegment(t).roadName;
                if (!ConvertUtils.isEmpty(roadName)) {
                    parentRoadName = roadName;
                }
                routeLineSegmentInfoParent.setLoadName(parentRoadName);
                //聚合 -- 道路距离
                String parentRoadLength = TimeUtils.getInstance().getDistanceString(info.getGroupSegment(t).length);
                routeLineSegmentInfoParent.setDistance(parentRoadLength);

                //聚合段 -- 开始索引
                int startSegmentIndex = info.getGroupSegment(t).startSegmentIndex;

                //聚合段 -- 转向ID
                int parentTypeID;
                parentTypeID = info.getSegmentInfo(startSegmentIndex).getMainAction();
                if (parentTypeID == MainAction.MainActionNULL) {
                    parentTypeID = info.getSegmentInfo(startSegmentIndex).getAssistantAction();
                    Logger.i(TAG, "AssistantAction_jh_line: " + parentTypeID);
                }
                routeLineSegmentInfoParent.setIconType(parentTypeID);

                //聚合段 -- 其中导航段数量
                int segmentCount = info.getGroupSegment(t).segmentCount;
                int lightCount = NumberUtils.NUM_0;
                List<Long> avoidRoadList = new ArrayList<>();
                List<RouteLineSegmentInfo> routeLineSegmentInfoChildList = new ArrayList<>();
                if (segmentCount > NumberUtils.NUM_0) {
                    for (int s = NumberUtils.NUM_0; s < segmentCount; s++) {
                        int currentSegmentIndex = s + startSegmentIndex;
                        lightCount += (int) info.getSegmentInfo(currentSegmentIndex).getTrafficLightNum();
                        avoidRoadList.addAll(getAvoidRoadList(info.getSegmentInfo(currentSegmentIndex)));
                        RouteLineSegmentInfo routeLineSegmentInfoChild = new RouteLineSegmentInfo();
                        String childRoadName = "无名路";
                        if (currentSegmentIndex < info.getSegmentCount() - NumberUtils.NUM_1 && !ConvertUtils.isEmpty(info.getSegmentInfo(currentSegmentIndex + NumberUtils.NUM_1).getLinkInfo(NumberUtils.NUM_0).getRoadName())) {
                            childRoadName = info.getSegmentInfo(currentSegmentIndex + NumberUtils.NUM_1).getLinkInfo(NumberUtils.NUM_0).getRoadName();
                        }

                        if (currentSegmentIndex == info.getSegmentCount() - NumberUtils.NUM_1) {
                            childRoadName = "终点";
                        }
                        //导航段 -- 下个路名
                        routeLineSegmentInfoChild.setLoadName(childRoadName);
                        //导航段 -- 当前红绿灯
                        routeLineSegmentInfoChild.setLightCount(lightCount);
                        //导航段 -- 距离
                        String childLength = TimeUtils.getInstance().getDistanceString(info.getSegmentInfo(currentSegmentIndex).getLength());
                        routeLineSegmentInfoChild.setDistance(childLength);
                        //导航段 -- 转向ID
                        int childTypeID = info.getSegmentInfo(currentSegmentIndex).getMainAction();
                        if (childTypeID == MainAction.MainActionNULL) {
                            childTypeID = info.getSegmentInfo(currentSegmentIndex).getAssistantAction();
                            Logger.i(TAG, "AssistantAction_dh_line: " + childTypeID);
                        }
                        routeLineSegmentInfoChild.setIconType(childTypeID);
                        routeLineSegmentInfoChildList.add(routeLineSegmentInfoChild);
                    }
                }
                //聚合段 -- 红绿灯总数
                routeLineSegmentInfoParent.setLightCount(lightCount);
                routeLineSegmentInfoParent.setAvoidList(avoidRoadList);

                routeLineSegmentInfoParent.setRouteLineSegmentInfos(routeLineSegmentInfoChildList);
                routeLineSegmentInfoParentList.add(routeLineSegmentInfoParent);
            }
        }
        return routeLineSegmentInfoParentList;
    }

    private List<Long> getAvoidRoadList(SegmentInfo segmentInfo) {
        List<Long> avoidRoadList = new ArrayList<>();
        if (segmentInfo.getLinkCount() <= NumberUtils.NUM_0) return avoidRoadList;
        for (int t = NumberUtils.NUM_0; t < segmentInfo.getLinkCount(); t++) {
            avoidRoadList.add(segmentInfo.getLinkInfo(t).get64TopoID().longValue());
        }
        return avoidRoadList;
    }

    private final IRouteResultObserver routeResultObserver = new IRouteResultObserver() {
        @Override
        public void onNewRoute(PathResultData pathResultData, ArrayList<PathInfo> pathInfoList, RouteLimitInfo routeLimitInfo) {
            Logger.i(TAG, "pathResultData -> " + pathResultData.errorCode, " pathInfoList -> " + pathInfoList.size(), " routeLimitInfo -> " + routeLimitInfo);
            handResultSuccess();
            if (ConvertUtils.isEmpty(pathResultData)) return;
            long requestId = pathResultData.requestId;
            mRequsetId = requestId;
            Logger.i(TAG, "route plane result id " + requestId);
            RequestRouteResult requestRouteResult = ConvertUtils.containToValue(routeResultDataHashtable, requestId);
            Logger.i(TAG, "从请求队列中获取结果对象 " + requestRouteResult);
            if (null == requestRouteResult) return;
            handlerRouteResult(requestRouteResult, pathInfoList);
            handlerDrawLine(requestRouteResult.getLineLayerParam(), pathInfoList, requestId, requestRouteResult.getMapTypeId(), requestRouteResult.isOnlineRoute());
            handlerRestriction(requestRouteResult.getRouteRestrictionParam(), pathInfoList, requestId, requestRouteResult.getMapTypeId(), requestRouteResult.isOnlineRoute());
            handlerRange(pathInfoList, requestRouteResult.isOnlineRoute());
            handlerCityAdCode(requestRouteResult.getRouteAlongCityParam(), pathInfoList, requestId, requestRouteResult.getMapTypeId(), requestRouteResult.isOnlineRoute());
            handlerChargingStation(requestRouteResult.getRouteChargeStationParam(), pathInfoList, requestId, requestRouteResult.getMapTypeId());
            handlerL2SData(pathInfoList);
//            handlerTrafficIncident(requestRouteResult.getRouteTrafficIncidentParam(), pathInfoList, requestId, requestRouteResult.getMapTypeId(), requestRouteResult.isOnlineRoute());
//            handlerRestTollGate(requestRouteResult.getRouteRestTollGateParam(), pathInfoList, requestId, requestRouteResult.getMapTypeId(), requestRouteResult.isOnlineRoute());
//            handlerRestArea(requestRouteResult.getRouteRestAreaParam(), pathInfoList, requestId, requestRouteResult.getMapTypeId(), requestRouteResult.isOnlineRoute());
        }

        @Override
        public void onNewRouteError(PathResultData pathResultData, RouteLimitInfo routeLimitInfo) {
            Logger.i(TAG, "pathResultData : ", pathResultData,
                    "routeLimitInfo : ", routeLimitInfo);
            if (ConvertUtils.isEmpty(routeResultObserverHashtable)) return;
            RequestRouteResult requestRouteResult = ConvertUtils.containToValue(routeResultDataHashtable, pathResultData.requestId);
            if (!ConvertUtils.isEmpty(requestRouteResult)) {
                String errorMsg = getErrorMsgs(requestRouteResult.getRouteWay());
                String errorMsgDetail = getErrorMsgsDetails(pathResultData.errorCode);
                for (RouteResultObserver resultObserver : routeResultObserverHashtable.values()) {
                    if (resultObserver == null) continue;
                    resultObserver.onRouteFail(requestRouteResult, pathResultData.errorCode, errorMsg + errorMsgDetail);
                }
            }
        }

        private String getErrorMsgs(int routeWay) {
            switch (routeWay) {
                case RouteWayID.ROUTE_WAY_DEFAULT:
                    return "算路失败";
                case RouteWayID.ROUTE_WAY_REFRESH:
                    return "刷新路线失败";
                case RouteWayID.ROUTE_WAY_AVOID:
                    return "避开道路失败";
                case RouteWayID.ROUTE_WAY_ADD_VIA:
                    return "途经点添加失败";
                case RouteWayID.ROUTE_WAY_DELETE_VIA:
                    return "途经点删除失败";
                case RouteWayID.ROUTE_WAY_SORT_VIA:
                    return "途经点排序失败";
            }
            return "算路失败";
        }

        private String getErrorMsgsDetails(int errorCode) {
            switch (errorCode) {
                case 822083601:
                case 822083607:
                case 822083608:
                    return "：该城市无离线数据";

            }
            return "";
        }
    };

    /**
     * 算路请求成功回调，用以提示HMI结束Loading框和清除队列里的空对象
     */
    private void handResultSuccess() {
        for (String key : routeResultObserverHashtable.keySet()) {
            RouteResultObserver resultObserver = routeResultObserverHashtable.get(key);
            if (resultObserver != null) resultObserver.onRouteSuccess();
            else routeResultObserverHashtable.remove(key);
        }
    }

    /**
     * 封装给HMI的路段聚合信息及路线详情
     *
     * @param requestRouteResult 路线结果类
     * @param pathInfoList       路线信息
     */
    private void handlerRouteResult(RequestRouteResult requestRouteResult, ArrayList<PathInfo> pathInfoList) {
        List<RouteLineInfo> routeResults = convertPathInfo2RouteResult(pathInfoList, requestRouteResult.isOnlineRoute());
        requestRouteResult.setRouteLineInfos(routeResults);
        Logger.i(TAG, "请求结果获取透传给HMI的信息 " + routeResults);
        for (RouteResultObserver resultObserver : routeResultObserverHashtable.values()) {
            if (resultObserver == null) continue;
            resultObserver.onRouteResult(requestRouteResult);
        }
    }

    /**
     * 路线绘制回调接口
     *
     * @param routeLineLayerParam 路线绘制参数
     * @param pathInfoList        路线信息
     * @param requestId           请求Id
     * @param mapTypeId           视图Id
     * @param onlineRoute
     */
    private void handlerDrawLine(RouteLineLayerParam routeLineLayerParam, ArrayList<PathInfo> pathInfoList, long requestId, MapTypeId mapTypeId, boolean onlineRoute) {
        routeLineLayerParam.setRequestId(requestId);
        routeLineLayerParam.setMapTypeId(mapTypeId);
        routeLineLayerParam.setOnlineRoute(onlineRoute);
        routeLineLayerParam.setPathInfoList(pathInfoList);
        Logger.i(TAG, "获取路线绘制的参数 " + GsonUtils.toJson(routeLineLayerParam));
        for (RouteResultObserver resultObserver : routeResultObserverHashtable.values()) {
            if (resultObserver == null) continue;
            resultObserver.onRouteDrawLine(routeLineLayerParam);
        }
    }

    /**
     * 路线上服务区数据回调
     *
     * @param routeRestAreaParam 服务区数据
     * @param pathInfoList       路线信息
     * @param requestId          请求Id
     * @param mapTypeId          视图Id
     * @param onlineRoute
     */
    private void handlerRestArea(RouteRestAreaParam routeRestAreaParam, ArrayList<PathInfo> pathInfoList, long requestId, MapTypeId mapTypeId, boolean onlineRoute) {
        routeRestAreaParam.setRequestId(requestId);
        routeRestAreaParam.setMapTypeId(mapTypeId);
        routeRestAreaParam.setOnlineRoute(onlineRoute);
        routeRestAreaParam.setPathInfoList(pathInfoList);
        ArrayList<RouteRestAreaInfo> routeRestAreaInfos = new ArrayList<>();
        for (PathInfo pathInfo : pathInfoList) {
            RouteRestAreaInfo routeRestAreaInfo = new RouteRestAreaInfo();
            ArrayList<RestAreaInfo> restAreas = pathInfo.getRestAreas(0, 100);
            List<RouteRestAreaDetailsInfo> routeRestAreaDetailsInfos = new ArrayList<>();
            for (RestAreaInfo info : restAreas) {
                RouteRestAreaDetailsInfo routeRestAreaDetailsInfo = GsonUtils.convertToT(info, RouteRestAreaDetailsInfo.class);
                routeRestAreaDetailsInfos.add(routeRestAreaDetailsInfo);
            }
            routeRestAreaInfo.setRouteRestAreaDetailsInfos(routeRestAreaDetailsInfos);
            routeRestAreaInfos.add(routeRestAreaInfo);
        }
        routeRestAreaParam.setRouteRestAreaInfos(routeRestAreaInfos);
        for (RouteResultObserver resultObserver : routeResultObserverHashtable.values()) {
            if (resultObserver == null) continue;
            resultObserver.onRouteRestAreaInfo(routeRestAreaParam);
        }
    }

    /**
     * 路线上充电站数据回调
     *
     * @param pathInfoList 路线信息
     * @param requestId    请求Id
     * @param mapTypeId    视图Id
     */
    private void handlerChargingStation(RouteChargeStationParam routeChargeStationParam, ArrayList<PathInfo> pathInfoList, long requestId, MapTypeId mapTypeId) {
        routeChargeStationParam.setRequestId(requestId);
        routeChargeStationParam.setMapTypeId(mapTypeId);
        ArrayList<RouteChargeStationInfo> ChargeStationInfos = new ArrayList<>();
        routeChargeStationParam.setPathInfoList(pathInfoList);
        for (PathInfo pathInfo : pathInfoList) {
            RouteChargeStationInfo routeChargeStationInfo = new RouteChargeStationInfo();
            ArrayList<RouteChargeStationDetailInfo> routeChargeStationDetailInfos = new ArrayList<>();
            if (pathInfo.getChargeStationInfo() == null || pathInfo.getChargeStationInfo().isEmpty()) {
                Logger.d("handlerChargingStation null");
                routeChargeStationInfo.setRouteChargeStationDetailInfo(routeChargeStationDetailInfos);
                ChargeStationInfos.add(routeChargeStationInfo);
                continue;
            }
            Logger.d("handlerChargingStation " + GsonUtils.toJson(pathInfo.getChargeStationInfo()));
            for (ChargeStationInfo chargeStationInfo : pathInfo.getChargeStationInfo()) {
                RouteChargeStationDetailInfo routeChargeStationDetailInfo = GsonUtils.convertToT(chargeStationInfo, RouteChargeStationDetailInfo.class);
                routeChargeStationDetailInfos.add(routeChargeStationDetailInfo);
            }
            routeChargeStationInfo.setRouteChargeStationDetailInfo(routeChargeStationDetailInfos);
            ChargeStationInfos.add(routeChargeStationInfo);
        }
        routeChargeStationParam.setRouteChargeStationInfos(ChargeStationInfos);
        for (RouteResultObserver resultObserver : routeResultObserverHashtable.values()) {
            if (resultObserver == null) continue;
            resultObserver.onRouteChargeStationInfo(routeChargeStationParam);
        }
    }

    /**
     * 路线上充电站数据回调
     *
     * @param routeAlternativeChargeStationResult 备选充电桩信息
     */
    private void handlerAlternativeChargingStation(RouteAlternativeChargeStationResult routeAlternativeChargeStationResult) {
        RouteAlterChargeStationParam routeAlterChargeStationParam = new RouteAlterChargeStationParam();
        routeAlterChargeStationParam.setRequestId(routeAlternativeChargeStationResult.taskId);
        ArrayList<RouteAlterChargeStationInfo> routeAlterChargeStationInfos = new ArrayList<>();
        if (!routeAlternativeChargeStationResult.alternativeChargeStationInfos.isEmpty()) {
            for (RouteAlternativeChargeStationInfo routeAlternativeChargeStationInfo : routeAlternativeChargeStationResult.alternativeChargeStationInfos) {
                RouteAlterChargeStationInfo routeAlterChargeStationInfo = GsonUtils.convertToT(routeAlternativeChargeStationInfo, RouteAlterChargeStationInfo.class);
                routeAlterChargeStationInfos.add(routeAlterChargeStationInfo);
            }
        }
        routeAlterChargeStationParam.setRouteAlternativeChargeStationInfos(routeAlterChargeStationInfos);
        for (RouteResultObserver resultObserver : routeResultObserverHashtable.values()) {
            if (resultObserver == null) continue;
            resultObserver.onRouteAlterChargeStationInfo(routeAlterChargeStationParam);
        }
    }

    /**
     * 路线上天气数据回调
     *
     * @param routeWeatherParam 天气总数据
     * @param weatherLabelItems 天气数据
     * @param requestId         请求Id
     * @param mapTypeId         视图Id
     */
    private void handlerWeather(RouteWeatherParam routeWeatherParam, ArrayList<WeatherLabelItem> weatherLabelItems, long requestId, MapTypeId mapTypeId) {
        if (ConvertUtils.isEmpty(weatherLabelItems) || weatherLabelItems.isEmpty()) {
            for (RouteResultObserver resultObserver : routeResultObserverHashtable.values()) {
                if (resultObserver == null) continue;
                resultObserver.onRouteWeatherInfo(null);
            }
            return;
        }
        routeWeatherParam.setRequestId(requestId);
        routeWeatherParam.setMapTypeId(mapTypeId);
        routeWeatherParam.setWeatherLabelItem(weatherLabelItems);
        ArrayList<RouteWeatherInfo> routeWeatherInfos = new ArrayList<>();
        for (WeatherLabelItem weatherLabelItem : weatherLabelItems) {
            RouteWeatherInfo routeWeatherInfo = GsonUtils.convertToT(weatherLabelItem, RouteWeatherInfo.class);
            routeWeatherInfos.add(routeWeatherInfo);

        }
        routeWeatherParam.setRouteWeatherInfos(routeWeatherInfos);
        for (RouteResultObserver resultObserver : routeResultObserverHashtable.values()) {
            if (resultObserver == null) continue;
            resultObserver.onRouteWeatherInfo(routeWeatherParam);
        }
    }

    /**
     * 限行提示
     *
     * @param routeRestrictionParam 限行总数据
     * @param pathInfoList          限行提示
     * @param requestId             请求Id
     * @param mapTypeId             视图Id
     * @param onlineRoute
     */
    private void handlerRestriction(RouteRestrictionParam routeRestrictionParam, ArrayList<PathInfo> pathInfoList, long requestId, MapTypeId mapTypeId, boolean onlineRoute) {
        if (pathInfoList.size() <= NumberUtils.NUM_0 || pathInfoList.get(NumberUtils.NUM_0).getRestrictionInfo().title.length() == NumberUtils.NUM_0) {
            return;
        }
        routeRestrictionParam.setRequestId(requestId);
        routeRestrictionParam.setMapTypeId(mapTypeId);
        routeRestrictionParam.setOnlineRoute(onlineRoute);
        List<RouteRestrictionInfo> routeRestrictionInfos = routeRestrictionParam.getRouteRestrictionInfo();
        List<String> ruleIds = routeRestrictionParam.getRuleIds();
        for (PathInfo pathInfo : pathInfoList) {
            RouteRestrictionInfo routeRestrictionInfo = GsonUtils.convertToT(pathInfo.getRestrictionInfo(), RouteRestrictionInfo.class);
            ruleIds.add(getRuleId(pathInfo.getRestrictionInfo().ruleIDs));
            routeRestrictionInfos.add(routeRestrictionInfo);
        }
        routeRestrictionParam.setRouteRestrictionInfo(routeRestrictionInfos);
        for (RouteResultObserver resultObserver : routeResultObserverHashtable.values()) {
            if (resultObserver == null) continue;
            resultObserver.onRouteRestrictionInfo(routeRestrictionParam);
        }
    }

    private String getRuleId(ArrayList<Long> ruleIdList) {
        String ruleIds = "";
        for (int t = 0; t < ruleIdList.size(); t++) {
            if (t == ruleIdList.size() - 1) {
                ruleIds += ruleIdList.get(t) + "";
            } else {
                ruleIds += ruleIdList.get(t) + "|";
            }

        }
        return ruleIds;
    }

    private RestrictedAreaDetail getRouteReStrictedAreaDetail(GReStrictedAreaResponseParam param) {
        RestrictedAreaDetail routeReStrictedAreaDetail = new RestrictedAreaDetail();
        if (ConvertUtils.isEmpty(param)) return routeReStrictedAreaDetail;
        if (ConvertUtils.isEmpty(param.data)) return routeReStrictedAreaDetail;
        if (ConvertUtils.isEmpty(param.data.mDataRule)) return routeReStrictedAreaDetail;
        if (param.data.mDataRule.cities.size() <= NumberUtils.NUM_0)
            return routeReStrictedAreaDetail;
        routeReStrictedAreaDetail.setTitle(param.data.mDataRule.cities.get(NumberUtils.NUM_0).title);
        routeReStrictedAreaDetail.setCityCode(param.data.mDataRule.cities.get(NumberUtils.NUM_0).cityCode);
        routeReStrictedAreaDetail.setCityName(param.data.mDataRule.cities.get(NumberUtils.NUM_0).cityName);
        if (param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.size() <= NumberUtils.NUM_0)
            return routeReStrictedAreaDetail;
        routeReStrictedAreaDetail.setDesc(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).desc);
        routeReStrictedAreaDetail.setEffect(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).effect);
        routeReStrictedAreaDetail.setLocal(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).local);
        routeReStrictedAreaDetail.setOtherdesc(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).otherdesc);
        routeReStrictedAreaDetail.setPolicyname(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).policyname);
        routeReStrictedAreaDetail.setRing(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).ring);
        routeReStrictedAreaDetail.setRuleid(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).ruleid);
        routeReStrictedAreaDetail.setSummary(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).summary);
        routeReStrictedAreaDetail.setTime(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).time);
        routeReStrictedAreaDetail.setVehicle(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).vehicle);
        return routeReStrictedAreaDetail;
    }


    /**
     * 沿途收费站
     *
     * @param routeRestTollGateParam 收费站数据
     * @param pathInfoList           路线信息
     * @param requestId              请求Id
     * @param mapTypeId              视图Id
     * @param onlineRoute
     */
    private void handlerRestTollGate(RouteRestTollGateParam routeRestTollGateParam, ArrayList<PathInfo> pathInfoList, long requestId, MapTypeId mapTypeId, boolean onlineRoute) {
        routeRestTollGateParam.setRequestId(requestId);
        routeRestTollGateParam.setMapTypeId(mapTypeId);
        routeRestTollGateParam.setOnlineRoute(onlineRoute);
        ArrayList<RouteRestTollGateInfo> routeRestTollGateInfos = new ArrayList<>();
        for (PathInfo pathInfo : pathInfoList) {
            RouteRestTollGateInfo routeRestTollGateInfo = new RouteRestTollGateInfo();
            List<RouteRestTollGateDetailsInfo> routeRestAreaDetailsInfos = new ArrayList<>();
            ArrayList<RestTollGateInfo> restTollGate = pathInfo.getRestTollGate(0, 100);
            for (RestTollGateInfo info : restTollGate) {
                RestTollGateInfo restTollGateInfo = GsonUtils.convertToT(info, RestTollGateInfo.class);
                restTollGate.add(restTollGateInfo);
            }
            routeRestTollGateInfo.setRouteRestTollGateDetailsInfos(routeRestAreaDetailsInfos);
            routeRestTollGateInfos.add(routeRestTollGateInfo);
        }
        routeRestTollGateParam.setRouteRestTollGateInfos(routeRestTollGateInfos);
        for (RouteResultObserver resultObserver : routeResultObserverHashtable.values()) {
            if (resultObserver == null) continue;
            resultObserver.onRouteRestTollGateInfo(routeRestTollGateParam);
        }
    }

    /**
     * 沿途城市列表
     *
     * @param routeAlongCityParam 城市列表数据
     * @param pathInfoList        路线信息
     * @param requestId           请求Id
     * @param mapTypeId           视图Id
     * @param onlineRoute
     */
    private void handlerCityAdCode(RouteAlongCityParam routeAlongCityParam, ArrayList<PathInfo> pathInfoList, long requestId, MapTypeId mapTypeId, boolean onlineRoute) {
        routeAlongCityParam.setRequestId(requestId);
        routeAlongCityParam.setMapTypeId(mapTypeId);
        routeAlongCityParam.setOnlineRoute(onlineRoute);
        ArrayList<RouteAlongCityInfo> routeAlongCityInfos = new ArrayList<>();
        for (PathInfo pathInfo : pathInfoList) {
            RouteAlongCityInfo routeAlongCityInfo = new RouteAlongCityInfo();
            routeAlongCityInfo.setAdCityList(pathInfo.getCityAdcodeList());
            routeAlongCityInfos.add(routeAlongCityInfo);
        }
        routeAlongCityParam.setRouteAlongCityInfos(routeAlongCityInfos);
        for (RouteResultObserver resultObserver : routeResultObserverHashtable.values()) {
            if (resultObserver == null) continue;
            resultObserver.onRouteCityInfo(routeAlongCityParam);
        }
    }

    /**
     * 沿途交通事件
     *
     * @param routeTrafficIncidentParam 收费站数据
     * @param pathInfoList              路线信息
     * @param requestId                 请求Id
     * @param mapTypeId                 视图Id
     * @param onlineRoute
     */
    private void handlerTrafficIncident(RouteTrafficIncidentParam routeTrafficIncidentParam, ArrayList<PathInfo> pathInfoList, long requestId, MapTypeId mapTypeId, boolean onlineRoute) {
        routeTrafficIncidentParam.setRequestId(requestId);
        routeTrafficIncidentParam.setMapTypeId(mapTypeId);
        routeTrafficIncidentParam.setOnlineRoute(onlineRoute);
        ArrayList<RouteTrafficIncidentInfo> routeTrafficIncidentInfos = new ArrayList<>();
        for (PathInfo pathInfo : pathInfoList) {
            RouteTrafficIncidentInfo routeTrafficIncidentInfo = new RouteTrafficIncidentInfo();
            ArrayList<RouteTrafficIncidentDetailsInfo> routeTrafficIncidentDetailsInfos = new ArrayList<>();
            for (int t = NumberUtils.NUM_0; t < pathInfo.getTrafficIncidentCount(false); t++) {
                RouteTrafficIncidentDetailsInfo routeTrafficIncidentDetailsInfo = GsonUtils.convertToT(pathInfo.getTrafficIncident((short) t, false), RouteTrafficIncidentDetailsInfo.class);
                routeTrafficIncidentDetailsInfos.add(routeTrafficIncidentDetailsInfo);
            }
            routeTrafficIncidentInfo.setRouteTrafficIncidentDetailsInfos(routeTrafficIncidentDetailsInfos);
            routeTrafficIncidentInfos.add(routeTrafficIncidentInfo);
        }
        routeTrafficIncidentParam.setRouteTrafficIncidentInfos(routeTrafficIncidentInfos);
        for (RouteResultObserver resultObserver : routeResultObserverHashtable.values()) {
            if (resultObserver == null) continue;
            resultObserver.onRouteTrafficIncidentInfo(routeTrafficIncidentParam);
        }
    }

    /**
     * 续航里程信息
     *
     * @param pathInfoList 路线信息
     * @param onlineRoute
     */
    private void handlerRange(ArrayList<PathInfo> pathInfoList, boolean onlineRoute) {
        ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos = new ArrayList<>();
        for (PathInfo pathInfo : pathInfoList) {
            EndPointEnergyInfo endPointInfo = pathInfo.getDynamicMergeInfo().endPointInfo;
            EvRangeOnRouteInfo evRangeOnRouteInfo = new EvRangeOnRouteInfo();
            evRangeOnRouteInfo.setCanArrived(endPointInfo.remainCapacity != -1);
            GeoPoint geoPoint = new GeoPoint();
            if (endPointInfo.energyEndFlag) {
                geoPoint.lat = endPointInfo.energyEndPoint.show.lat;
                geoPoint.lon = endPointInfo.energyEndPoint.show.lon;
            } else {
                geoPoint.lat = pathInfo.getEndPoi().naviPos.lat;
                geoPoint.lon = pathInfo.getEndPoi().naviPos.lon;
            }
            evRangeOnRouteInfo.setPos(geoPoint);

            //TODO 剩余电量计算剩余里程（当前坐标计算距离）
            long rangeDistance = TimeUtils.getInstance().calculateDistance(geoPoint.lat, geoPoint.lon,
                    pathInfo.getEndPoi().naviPos.lat, pathInfo.getEndPoi().naviPos.lon);
            evRangeOnRouteInfo.setRemainRangeDistance(rangeDistance);
            evRangeOnRouteInfos.add(evRangeOnRouteInfo);
        }
        Logger.i(TAG, "获取续航里程信息： " + evRangeOnRouteInfos);
        for (RouteResultObserver resultObserver : routeResultObserverHashtable.values()) {
            if (resultObserver == null) continue;
            resultObserver.onRouteRanges(evRangeOnRouteInfos);
        }
    }

    /**
     * 路线上充电站数据回调
     *
     * @param pathInfoList 路线信息
     */
    private void handlerL2SData(ArrayList<PathInfo> pathInfoList) {
        List<RouteL2Data> routeL2DataList = new ArrayList<>();
        for (PathInfo pathInfo : pathInfoList) {
            RouteL2Data routeL2Data = getRouteL2Data(pathInfo);
            routeL2DataList.add(routeL2Data);
        }
        for (RouteResultObserver resultObserver : routeResultObserverHashtable.values()) {
            if (resultObserver == null) continue;
            resultObserver.onRouteL2Info(GsonUtils.toJson(routeL2DataList));
        }
    }

    private RouteL2Data getRouteL2Data(PathInfo pathInfo) {
        RouteL2Data data = new RouteL2Data();
        data.engineVersion = "1.0";
        data.sdkVersion = "750";
        data.pathID = (int) pathInfo.getPathID();
        long segmentCount = pathInfo.getSegmentCount();
        int linkCnt = 0;
        int pntCnt = 0;
        List<RouteL2Data.LinksDTO> linksDTOS = new ArrayList<>();
        List<RouteL2Data.PntsDTO> pntsDTOS = new ArrayList<>();
        int firstIcon = 0;
        if (segmentCount > 0) {
            for (int segmentIndex = 0; segmentIndex < segmentCount; segmentIndex++) {
                //linkCnt
                long linkCount = pathInfo.getSegmentInfo(segmentIndex).getLinkCount();
                linkCnt += linkCount;
                int startIndex = -1;
                if (linkCnt > 0) {
                    for (int linkIndex = 0; linkIndex < linkCount; linkIndex++) {
                        LinkInfo linkInfo = pathInfo.getSegmentInfo(segmentIndex).getLinkInfo(linkIndex);
                        //linkCnt
                        pntCnt += linkInfo.getPoints().size();
                        //linksDTOS
                        RouteL2Data.LinksDTO dto = new RouteL2Data.LinksDTO();
                        // TrafficLightsDTO
                        dto.linkID = (int) linkInfo.getTPID();
                        dto.formway = linkInfo.getFormway();
                        dto.len = linkInfo.getLength();
                        dto.linktype = linkInfo.getLinkType();
                        dto.roadclass = linkInfo.getRoadClass();
                        dto.roadname = linkInfo.getRoadName();
                        dto.isToll = linkInfo.isToll();
                        dto.adminCode = (int) linkInfo.getAdcode();
                        dto.HasMixFork = linkInfo.hasMixFork();
                        dto.HasTrafficLight = linkInfo.hasTrafficLight();
                        dto.HasMultiOut = linkInfo.hasMultiOut();
                        dto.mainAction = linkInfo.getMainAction();
                        dto.assistantAction = linkInfo.getAssistantAction();
                        dto.hasParallel = linkInfo.hasParallelRoad();
                        dto.direction = (int) linkInfo.getRoadDirection();
                        dto.pntBegIdx = startIndex + 1;
                        dto.pntCnt = linkInfo.getPoints().size();
                        startIndex += linkInfo.getPoints().size();
                        linksDTOS.add(dto);
                        ArrayList<Coord2DInt32> points = linkInfo.getPoints();
                        if (points.size() > 0) {
                            for (int poiIndex = 0; poiIndex < points.size(); poiIndex++) {
                                RouteL2Data.PntsDTO pnt = new RouteL2Data.PntsDTO();
                                pnt.x = points.get(poiIndex).lon / 3600000.0;
                                pnt.y = points.get(poiIndex).lat / 3600000.0;
                                pntsDTOS.add(pnt);
                            }
                        }
                        firstIcon = linkInfo.getMainAction();
                    }
                }

            }
        }

        data.linkCnt = linkCnt;
        data.pntCnt = pntCnt;
        data.links = linksDTOS;
        data.pnts = pntsDTOS;

        List<RouteL2Data.GuideGroupsDTO> guideGroupsDTOS = new ArrayList<>();
        int groupSegmentCount = (int) pathInfo.getGroupSegmentCount();
        if (groupSegmentCount > 0) {
            for (int groupIndex = 0; groupIndex < groupSegmentCount; groupIndex++) {
                GroupSegment groupSegment = pathInfo.getGroupSegment(groupIndex);
                RouteL2Data.GuideGroupsDTO groupsDTO = new RouteL2Data.GuideGroupsDTO();
                RouteL2Data.GuideGroupsDTO.GroupEnterCoordDTO enterCoordDTO = new RouteL2Data.GuideGroupsDTO.GroupEnterCoordDTO();
                RouteL2Data.GuideGroupsDTO.GroupEnterCoordDTO.MapDTO mapDTO = new RouteL2Data.GuideGroupsDTO.GroupEnterCoordDTO.MapDTO();
                groupsDTO.groupIconType = firstIcon;
                groupsDTO.groupLen = (int) groupSegment.length;
                groupsDTO.groupName = groupSegment.roadName;
                int num = groupSegment.segmentCount;
                int index = groupSegment.startSegmentIndex;
                int lightSize = 0;
                int travelTime = 0;
                List<RouteL2Data.GuideGroupsDTO.SegmentsDTO> segmentsDTOList = new ArrayList<>();
                if (num > 0) {
                    mapDTO.x = pathInfo.getSegmentInfo(index).getLinkInfo(0).getPoints().get(0).lon / 3600000.0;
                    mapDTO.y = pathInfo.getSegmentInfo(index).getLinkInfo(0).getPoints().get(0).lat / 3600000.0;
                    enterCoordDTO.map = mapDTO;
                    groupsDTO.groupEnterCoord = enterCoordDTO;
                    for (int lightIndex = 0; lightIndex < num; lightIndex++) {
                        SegmentInfo segmentInfo = pathInfo.getSegmentInfo(index + lightIndex);
                        lightSize += segmentInfo.getTrafficLightNum();
                        RouteL2Data.GuideGroupsDTO.SegmentsDTO dto = new RouteL2Data.GuideGroupsDTO.SegmentsDTO();
                        dto.crntSegmLinkCnt = (int) segmentInfo.getLinkCount();
                        dto.linkBegIdx = segmentInfo.getSegmentIndex();
                        dto.description = "行驶" + TimeUtils.getInstance().getDistanceString(segmentInfo.getLength()) + iconId2String(segmentInfo) + "进入" + segmentInfo.getLinkInfo(0).getRoadName();
                        dto.isArriveWayPoint = groupSegment.isViaPoint;
                        dto.navigationMainAction = segmentInfo.getMainAction();
                        dto.navigationAssistAction = segmentInfo.getAssistantAction();
                        dto.navigationNextRoadName = segmentInfo.getCrossingName();
                        dto.navigationLen = (int) segmentInfo.getLength();
                        dto.trafficLightNum = (int) segmentInfo.getTrafficLightNum();
                        dto.travelTime = (int) segmentInfo.getTravelTime();
                        travelTime += (int) segmentInfo.getTravelTime();
                        segmentsDTOList.add(dto);
                    }
                }
                groupsDTO.groupTrafficLightsCount = lightSize;
                groupsDTO.groupTime = travelTime;
                groupsDTO.segments = segmentsDTOList;
                guideGroupsDTOS.add(groupsDTO);
            }
        }
        data.guideGroups = guideGroupsDTOS;

        ArrayList<Coord2DDouble> allTrafficLights = pathInfo.getAllTrafficLights();
        List<RouteL2Data.TrafficLightsDTO> trafficLightsDTOS = new ArrayList<>();
        if (allTrafficLights.size() > 0) {
            for (Coord2DDouble coord2DDouble : allTrafficLights) {
                RouteL2Data.TrafficLightsDTO dto = new RouteL2Data.TrafficLightsDTO();
                dto.x = coord2DDouble.lon;
                dto.y = coord2DDouble.lat;
                trafficLightsDTOS.add(dto);
            }
        }
        data.trafficLights = trafficLightsDTOS;

        ArrayList<RestTollGateInfo> restTollGate = pathInfo.getRestTollGate(0, 20);
        List<RouteL2Data.RestTollGateInfosDTO> restTollGateInfosDTOS = new ArrayList<>();
        if (restTollGate.size() > 0) {
            for (RestTollGateInfo info : restTollGate) {
                RouteL2Data.RestTollGateInfosDTO dto = new RouteL2Data.RestTollGateInfosDTO();
                dto.remainDist = (int) info.remainDist;
                dto.remainTime = (int) info.remainTime;
                dto.tollGateName = info.TollGateName;
                RouteL2Data.RestTollGateInfosDTO.PosDTO pos = new RouteL2Data.RestTollGateInfosDTO.PosDTO();
                pos.x = info.pos.lon;
                pos.y = info.pos.lat;
                dto.pos = pos;
                restTollGateInfosDTOS.add(dto);
            }
        }
        data.restTollGateInfos = restTollGateInfosDTOS;

        List<RouteL2Data.ViaRoadsDTO> viaRoadsDTOS = new ArrayList<>();
        int maxLane = 0;
        int minLane = 0;
        int maxSpeed = 0;
        int minSpeed = 0;
        int roadClass = 0;
        if (segmentCount > 0) {
            for (int segmentIndex = 0; segmentIndex < segmentCount; segmentIndex++) {
                GroupSegment groupSegment = pathInfo.getGroupSegment(segmentIndex);
                if (groupSegment.isViaPoint) {
                    RouteL2Data.ViaRoadsDTO dto = new RouteL2Data.ViaRoadsDTO();
                    dto.roadName = groupSegment.roadName;
                    dto.length = (int) groupSegment.length;
                    int startIndex = groupSegment.startSegmentIndex;
                    int count = groupSegment.segmentCount;
                    if (count > 0) {
                        RouteL2Data.ViaRoadsDTO.CoordinateDTO coordinateDTO = new RouteL2Data.ViaRoadsDTO.CoordinateDTO();
                        roadClass = pathInfo.getSegmentInfo(startIndex).getLinkInfo(0).getRoadClass();
                        dto.roadClass = roadClass;
                        Coord2DInt32 coord2DInt32 = pathInfo.getSegmentInfo(startIndex).getPoints().get(0);
                        coordinateDTO.x = coord2DInt32.lon / 3600000.0;
                        coordinateDTO.y = coord2DInt32.lat / 3600000.0;
                        dto.coordinate = coordinateDTO;
                        for (int t = 0; t < count; t++) {
                            SegmentInfo segmentInfo = pathInfo.getSegmentInfo(startIndex + t);
                            short speed = segmentInfo.getLinkInfo(0).getSpeed();
                            if (t == 0) {
                                maxSpeed = speed;
                                minSpeed = speed;
                            }
                            if (speed >= maxSpeed) {
                                maxSpeed = speed;
                            }
                            if (speed <= minSpeed) {
                                minSpeed = speed;
                            }
                            short laneNum = segmentInfo.getLinkInfo(0).getLaneNum();
                            if (t == 0) {
                                maxLane = laneNum;
                                minLane = laneNum;
                            }
                            if (laneNum >= maxLane) {
                                maxLane = laneNum;
                            }
                            if (laneNum <= minLane) {
                                minLane = laneNum;
                            }
                        }
                    }
                    dto.minSpeedLimit = minSpeed;
                    dto.maxSpeedLimit = maxSpeed;
                    dto.minLaneNum = minLane;
                    dto.maxLaneNum = maxLane;
                    viaRoadsDTOS.add(dto);
                }
            }
        }
        data.viaRoads = viaRoadsDTOS;

        RouteL2Data.EndPoiDTO endPoiDTO = new RouteL2Data.EndPoiDTO();
        endPoiDTO.id = mPoiInfoEntityEnd.getPid();
        endPoiDTO.name = mPoiInfoEntityEnd.getName();
        endPoiDTO.type = mPoiInfoEntityEnd.getPoiType();
        if (!ConvertUtils.isEmpty(mPoiInfoEntityEnd)) {
            List<RouteL2Data.EndPoiDTO.EntranceListDTO> entranceListDTOS = new ArrayList<>();
            List<RouteL2Data.EndPoiDTO.ExitListDTO> exitListDTOS = new ArrayList<>();
            List<RouteL2Data.EndPoiDTO.ParkingInfoListDTO> parkingInfoListDTOS = new ArrayList<>();
            if (!ConvertUtils.isEmpty(mPoiInfoEntityEnd) && !ConvertUtils.isEmpty(mPoiInfoEntityEnd.getParkingInfoList()) && mPoiInfoEntityEnd.getParkingInfoList().size() > 0) {
                for (ParkingInfo info : mPoiInfoEntityEnd.getParkingInfoList()) {
                    RouteL2Data.EndPoiDTO.ParkingInfoListDTO parkingInfoListDTO = new RouteL2Data.EndPoiDTO.ParkingInfoListDTO();
                    parkingInfoListDTO.id = info.getPoiId();
                    parkingInfoListDTO.name = info.getName();
                    parkingInfoListDTO.type = info.getQueryType();
                    List<RouteL2Data.EndPoiDTO.ParkingInfoListDTO.EntranceListDTO> entranceListDTOArrayList = new ArrayList<>();
                    List<RouteL2Data.EndPoiDTO.ParkingInfoListDTO.ExitListDTO> exitListDTOArrayList = new ArrayList<>();
                    if (!ConvertUtils.isEmpty(info) && !ConvertUtils.isEmpty(info.getSearchParkInOutInfos()) && !info.getSearchParkInOutInfos().isEmpty()) {
                        for (SearchParkInOutInfo searchParkInOutInfo : info.getSearchParkInOutInfos()) {
                            RouteL2Data.EndPoiDTO.EntranceListDTO dto = new RouteL2Data.EndPoiDTO.EntranceListDTO();
                            RouteL2Data.EndPoiDTO.ExitListDTO exitListDTO = new RouteL2Data.EndPoiDTO.ExitListDTO();
                            dto.x = searchParkInOutInfo.getX();
                            dto.y = searchParkInOutInfo.getY();
                            exitListDTO.x = searchParkInOutInfo.getX();
                            exitListDTO.y = searchParkInOutInfo.getY();
                            entranceListDTOS.add(dto);
                            exitListDTOS.add(exitListDTO);

                            RouteL2Data.EndPoiDTO.ParkingInfoListDTO.EntranceListDTO entranceListDTO = new RouteL2Data.EndPoiDTO.ParkingInfoListDTO.EntranceListDTO();
                            RouteL2Data.EndPoiDTO.ParkingInfoListDTO.ExitListDTO exitListDTOBean = new RouteL2Data.EndPoiDTO.ParkingInfoListDTO.ExitListDTO();
                            entranceListDTO.x = searchParkInOutInfo.getX();
                            entranceListDTO.y = searchParkInOutInfo.getY();
                            exitListDTOBean.x = searchParkInOutInfo.getX();
                            exitListDTOBean.y = searchParkInOutInfo.getY();
                            entranceListDTOArrayList.add(entranceListDTO);
                            exitListDTOArrayList.add(exitListDTOBean);
                        }
                    }
                    parkingInfoListDTO.entranceList = entranceListDTOArrayList;
                    parkingInfoListDTO.exitList = exitListDTOArrayList;
                    parkingInfoListDTOS.add(parkingInfoListDTO);
                }
            }
        }
        data.endPoi = endPoiDTO;
        return data;
    }

    private String iconId2String(SegmentInfo segmentInfo) {
        switch (segmentInfo.getMainAction()) {
            case 1:
                return "左转";
            case 2:
                return "右转";
            case 3:
                return "向左前方行驶";
            case 4:
                return "向右前方行驶";
            case 5:
                return "向左后方行驶";
            case 6:
                return "向右后方行驶";
            case 7:
                return "左转调头";
            case 8:
                return "直行";
            case 9:
                return "靠左";
            case 10:
                return "靠右";
            case 11:
                return "进入环岛";
            case 12:
                return "离开环岛";
        }

        return "直行";
    }

    private final IRouteWeatherObserver routeWeatherObserver = (requestId, arrayList) -> {
        Logger.i(TAG, "requestId -> " + requestId, "arrayList -> " + arrayList);
        RequestRouteResult requestRouteResult = ConvertUtils.containToValue(routeResultDataHashtable, mRequsetId);
        handlerWeather(requestRouteResult.getRouteWeatherParam(), arrayList, requestId, requestRouteResult.getMapTypeId());
    };

    private final IRouteAlternativeChargeStationObserver routeAlternativeChargeStationObserver = new IRouteAlternativeChargeStationObserver() {
        @Override
        public void onResult(RouteAlternativeChargeStationResult routeAlternativeChargeStationResult) {
            Logger.i(TAG, " requestId -> " + routeAlternativeChargeStationResult.taskId);
            handlerAlternativeChargingStation(routeAlternativeChargeStationResult);
        }
    };

    public IRouteAlternativeChargeStationObserver getRouteAlternativeChargeStationObserver() {
        return routeAlternativeChargeStationObserver;
    }

    public void sendEndEntity(PoiInfoEntity poiInfoEntity) {
        mPoiInfoEntityEnd = poiInfoEntity;
    }
}