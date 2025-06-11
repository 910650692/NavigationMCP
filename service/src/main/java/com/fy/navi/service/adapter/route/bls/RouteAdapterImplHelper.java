package com.fy.navi.service.adapter.route.bls;


import android.util.Log;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.TimeUtils;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.common.model.Coord2DInt32;
import com.autonavi.gbl.common.path.model.ChargingArgumentsInfo;
import com.autonavi.gbl.common.path.model.GroupSegment;
import com.autonavi.gbl.common.path.model.LightBarItem;
import com.autonavi.gbl.common.path.model.RestrictionInfo;
import com.autonavi.gbl.common.path.model.TrafficIncident;
import com.autonavi.gbl.common.path.option.LinkInfo;
import com.autonavi.gbl.route.model.BLRerouteRequestInfo;
import com.autonavi.gbl.route.observer.INaviRerouteObserver;
import com.fy.navi.service.define.route.Coord3DDouble;
import com.fy.navi.service.define.route.RouteAlterChargePriceInfo;
import com.fy.navi.service.define.route.RouteAlternativeChargeDetourInfo;
import com.fy.navi.service.define.route.RouteChargeStationNumberInfo;
import com.fy.navi.service.define.route.RouteCurrentPathParam;
import com.fy.navi.service.define.route.RouteL2Data;
import com.fy.navi.service.define.route.RouteLightBarItem;
import com.fy.navi.service.define.route.RoutePriorityType;
import com.fy.navi.service.define.route.RouteTMCParam;
import com.fy.navi.service.define.route.RouteWeatherID;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchParkInOutInfo;
import com.fy.navi.service.define.utils.BevPowerCarUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.aosclient.BLAosService;
import com.autonavi.gbl.aosclient.model.GReStrictedAreaResponseParam;
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
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.route.RouteResultObserver;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapType;
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
import com.fy.navi.service.define.calibration.PowerType;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.signal.SignalPackage;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

/**
 * RouteService辅助类.
 *
 * @author lvww
 * @version \$Revision.1.0\$
 * Description Helper类只做对象及数据转换，不做原子能力调用
 * Date 2024/12/5
 */
public class RouteAdapterImplHelper {
    private static final String TAG = MapDefaultFinalTag.ROUTE_SERVICE_TAG;
    private final RouteService mRouteService;
    /*** 该集合只存放算路结果回调 key = 哪个类请求的，value = requestResult，理论上集合的长度永远为1 **/
    private final Hashtable<String, RouteResultObserver> mRouteResultObserverHashtable;
    /*** 算路请求队列，会初始化一些的到结果前的一些参数 key = requestId，value = requestResult **/
    private Hashtable<Long, RequestRouteResult> mRouteResultDataHashtable;
    private long mRequsetId = -1;

    private int mRouteStrategy;
    private int mRouteConstrainCode;
    private UserAvoidInfo mUserAvoidInfo;
    private PoiInfoEntity mPoiInfoEntityEnd;

    protected RouteAdapterImplHelper(final RouteService routeService, final BLAosService blAosService) {
        mRouteResultObserverHashtable = new Hashtable<>();
        mRouteResultDataHashtable = new Hashtable<>();
        mRouteService = routeService;
        mUserAvoidInfo = new UserAvoidInfo();
    }

    protected void initRouteService() {
        mRouteService.init(getRouteServiceParam());
        mRouteService.addRouteResultObserver(mRouteResultObserver);
        mRouteService.addRouteWeatherObserver(mRouteWeatherObserver);
        mRouteService.addRerouteObserver(mRerouteObserver);
    }

    /**
     * 获取算路结果
     *
     * @return 算路结果
     */
    public Hashtable<Long, RequestRouteResult> getRouteResultDataHashtable() {
        if (ConvertUtils.isEmpty(mRouteResultDataHashtable)) {
            mRouteResultDataHashtable = new Hashtable<>();
        }
        return mRouteResultDataHashtable;
    }

    protected RouteInitParam getRouteServiceParam() {
        final RouteInitParam routeInitParam = new RouteInitParam();
        //所有的重算由HMI自行发起
        routeInitParam.collisionParam.state = RouteSerialParallelState.RouteSerial;
        routeInitParam.collisionParam.solution = RouteCollisionSolution.DiceCollision;
        routeInitParam.rerouteParam.enableAutoReroute = true;
        routeInitParam.rerouteParam.enableAutoSwitchParallelReroute = false;//关闭“切换平行路自动重算”
        return routeInitParam;
    }

    /**
     * 注册算路回调监听
     *
     * @param key                 key
     * @param routeResultObserver 回调监听对象
     */
    public void registerRouteObserver(final String key, final RouteResultObserver routeResultObserver) {
        mRouteResultObserverHashtable.put(key, routeResultObserver);
        Logger.d(TAG, "registerRouteObserver : " + mRouteResultObserverHashtable.size());
    }

    /**
     * 设置算路偏好
     *
     * @param routePreferenceID 偏好Id
     */
    public void setRoutePreference(final RoutePreferenceID routePreferenceID) {
        switch (routePreferenceID) {
            case PREFERENCE_RECOMMEND:
                mRouteStrategy = RouteStrategy.RouteStrategyPersonalGaodeBest;
                mRouteConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_AVOIDCONGESTION:
                mRouteStrategy = RouteStrategy.RouteStrategyPersonalTMC;
                mRouteConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_LESSCHARGE:
                mRouteStrategy = RouteStrategy.RouteStrategyPersonalLessMoney;
                mRouteConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_NOTHIGHWAY:
                mRouteStrategy = RouteStrategy.RouteStrategyPersonalLessHighway;
                mRouteConstrainCode = RouteConstrainCode.RouteAvoidFreeway | RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_FIRSTHIGHWAY:
                mRouteStrategy = RouteStrategy.RouteStrategyPersonalHighwayFirst;
                mRouteConstrainCode = RouteConstrainCode.RouteFreewayStrategy | RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_FIRSTMAINROAD:
                mRouteStrategy = RouteStrategy.RouteStrategyPersonalWidthFirst;
                mRouteConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_FASTESTSPEED:
                mRouteStrategy = RouteStrategy.RouteStrategyPersonalSpeedFirst;
                mRouteConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE:
                mRouteStrategy = RouteStrategy.RouteStrategyPersonalTMC2LessMoney;
                mRouteConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_NOTHIGHWAY:
                mRouteStrategy = RouteStrategy.RouteStrategyPersonalTMC2LessHighway;
                mRouteConstrainCode = RouteConstrainCode.RouteAvoidFreeway | RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTHIGHWAY:
                mRouteStrategy = RouteStrategy.RouteStrategyPersonalTMC2Highway;
                mRouteConstrainCode = RouteConstrainCode.RouteFreewayStrategy | RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_LESSCHARGE_AND_NOTHIGHWAY:
                mRouteStrategy = RouteStrategy.RouteStrategyPersonalLessMoney2LessHighway;
                mRouteConstrainCode = RouteConstrainCode.RouteAvoidFreeway | RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE_AND_NOTHIGHWAY:
                mRouteStrategy = RouteStrategy.RouteStrategyPersonalTMC2LessMondy2LessHighway;
                mRouteConstrainCode = RouteConstrainCode.RouteAvoidFreeway | RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTMAINROAD:
                mRouteStrategy = RouteStrategy.RouteStrategyPersonalTMC2WidthFirst;
                mRouteConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_FASTESTSPEED:
                mRouteStrategy = RouteStrategy.RouteStrategyPersonalTMC2SpeedFirst;
                mRouteConstrainCode = RouteConstrainCode.RouteCalcMulti;
                break;
            default:
                break;
        }
    }

    /**
     * 转化算路类别
     *
     * @param routePriorityType 算路Type
     * @return 算路Type
     */
    public int getRouteType(final int routePriorityType) {
        switch (routePriorityType) {
            case RoutePriorityType.ROUTE_TYPE_COMMON:
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
            default:
                return RouteType.RouteTypeCommon;
        }
    }

    public void setAvoidRoad(final RouteAvoidInfo routeAvoidInfo) {
        mUserAvoidInfo.linkList = routeAvoidInfo.getMAvoidList();
    }

    /**
     * 获取请求参数
     *
     * @param requestRouteResult 算路结果类
     * @param paramList          算路点参数
     * @return 算路条件
     */
    public RouteOption getRequestParam(final RequestRouteResult requestRouteResult, final List<RouteParam> paramList) {
        final RouteOption routeOption = new RouteOption();
        final POIForRequest poiForRequest = convertParam2PoiForRequest(requestRouteResult, paramList);
        routeOption.setPOIForRequest(poiForRequest);//设置算路行程点
        if (requestRouteResult.isMIsOnlineRoute()) {
            routeOption.setRouteStrategy(mRouteStrategy);
            routeOption.setRouteType(getRouteType(requestRouteResult.getMRouteType()));//算路类型
            routeOption.setUserAvoidInfo(mUserAvoidInfo);
            int constrainCode = mRouteConstrainCode;
            if (BevPowerCarUtils.getInstance().isElecPlanRoute) {
                Logger.d(TAG, "RouteElecContinue");
                // 1.打开电动车接续算路（主路线上有充电桩）
                constrainCode |= RouteConstrainCode.RouteElecContinue;
                // 2.打开电动车接续多备选算路（所有路线上有充电桩）
                constrainCode |= RouteConstrainCode.RouteMultiContinueCalc;
                routeOption.setConstrainCode(constrainCode);
            }
            routeOption.setVehicleCharge(BevPowerCarUtils.getInstance().initlialHVBattenergy);
            mUserAvoidInfo.linkList.clear();
        } else {
            routeOption.setRouteType(getRouteType(requestRouteResult.getMRouteType()));//算路类型
            routeOption.setRouteStrategy(mRouteStrategy);
            routeOption.setConstrainCode(mRouteConstrainCode | RouteConstrainCode.RouteCalcLocal);
            routeOption.setUserAvoidInfo(mUserAvoidInfo);
            routeOption.setOfflineReqCustomIdentityId("offline_request_id");
            routeOption.setVehicleCharge(BevPowerCarUtils.getInstance().initlialHVBattenergy);
            mUserAvoidInfo.linkList.clear();
        }
        return routeOption;
    }

    /**
     * 解注册算路回调监听
     *
     * @param key key
     */
    public void removeRouteObserver(final String key) {
        mRouteResultObserverHashtable.remove(key);
        Logger.d(TAG, "removeRouteObserver : " + mRouteResultObserverHashtable.size());
    }


    /**
     * 解除所有回调监听
     */
    public void removeAllObserver() {
        mRouteResultObserverHashtable.clear();
        Logger.d(TAG, "removeAllObserver : " + mRouteResultObserverHashtable.size());
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
    private POIForRequest convertParam2PoiForRequest(final RequestRouteResult requestRouteResult, final List<RouteParam> paramList) {
        final POIForRequest poiForRequest = new POIForRequest();
        final RouteLineLayerParam routeLineLayerParam = requestRouteResult.getMLineLayerParam();
        for (RouteParam routeParam : paramList) {
            final POIInfo poiInfo = getPOIInfo(routeParam);
            Logger.i(TAG, "poiInfo -> ", poiInfo);
            switch (routeParam.getPoiType()) {
                case RoutePoiType.ROUTE_POI_TYPE_START -> {
                    final RoutePoint routePoint = new RoutePoint();
                    routePoint.setMPos(routeParam.getRealPos());
                    routePoint.setMType(0);
                    routeLineLayerParam.getMRouteLinePoints().getMStartPoints().add(routePoint);
                    poiForRequest.addPoint(PointType.PointTypeStart, poiInfo);
                }
                case RoutePoiType.ROUTE_POI_TYPE_WAY -> {
                    final RoutePoint routePoint = new RoutePoint();
                    routePoint.setMPos(routeParam.getRealPos());
                    routePoint.setMType(2);
                    routeLineLayerParam.getMRouteLinePoints().getMViaPoints().add(routePoint);
                    poiForRequest.addPoint(PointType.PointTypeVia, poiInfo);
                }
                case RoutePoiType.ROUTE_POI_TYPE_END -> {
                    final RoutePoint routePoint = new RoutePoint();
                    routePoint.setMPos(routeParam.getRealPos());
                    routePoint.setMType(1);
                    routeLineLayerParam.getMRouteLinePoints().getMEndPoints().add(routePoint);
                    poiForRequest.addPoint(PointType.PointTypeEnd, poiInfo);
                }
                default -> Logger.i(TAG, "Poi点类型错误");
            }
        }
        routeLineLayerParam.setMPoiForRequest(poiForRequest);
        routeLineLayerParam.setMRouteType(requestRouteResult.getMRouteType());
        routeLineLayerParam.setMStrategy(mRouteStrategy);
        return poiForRequest;
    }

    /**
     * 转化点信息
     *
     * @param routeParam 算路点参数
     * @return 点信息
     */
    private POIInfo getPOIInfo(final RouteParam routeParam) {
        final POIInfo info = new POIInfo();
        info.realPos = new Coord2DDouble(routeParam.getRealPos().getLon(), routeParam.getRealPos().getLat());
        info.naviPos = new Coord2DDouble();
        info.sigshelter = routeParam.getSigshelter();
        info.type = routeParam.getType();
        info.roadID = routeParam.getRoadID();
        info.poiID = routeParam.getPoiID();
        info.name = routeParam.getName();
        info.floorName = "";
        info.parentName = "";
        info.parentSimpleName = "";
        info.angel = "";
        info.points = new ArrayList();
        info.parentID = "";
        info.parentRel = "";
        info.typeCode = routeParam.getTypeCode();
        info.fromJump = routeParam.getFromJump();
        info.overhead = routeParam.getOverhead();
        info.checkPointLinkId = new BigInteger("0");
        info.overheadBackAltDiff = routeParam.getOverheadBackAltDiff();
        info.floor = routeParam.getFloor();
        info.extendInfoFlag = routeParam.getExtendInfoFlag();
        info.srcApp = routeParam.getSrcApp();
        info.address = routeParam.getAddress();
        info.chargeInfo = new ChargingArgumentsInfo();
        info.retainParam = routeParam.getRetainParam();
        return info;
    }

    /**
     * 转化算路信息
     *
     * @param pathInfoList 算路参数
     * @param onlineRoute  是否在线算路
     * @param restoration  是否算路还原
     * @return 算路信息
     */
    private List<RouteLineInfo> convertPathInfo2RouteResult(final ArrayList<PathInfo> pathInfoList, final boolean onlineRoute,
                                                            final boolean restoration) {
        return getRouteLineInfoList(pathInfoList, onlineRoute, restoration);
    }

    /**
     * 路线基本信息
     *
     * @param pathInfoList 算路参数
     * @param onlineRoute  是否在线算路
     * @param restoration  是否算路还原
     * @return 算路信息
     **/
    private List<RouteLineInfo> getRouteLineInfoList(final ArrayList<PathInfo> pathInfoList, final boolean onlineRoute, final boolean restoration) {
        final List<RouteLineInfo> routeResults = new ArrayList<>();
        if (!ConvertUtils.isEmpty(pathInfoList)) {
            for (PathInfo info : pathInfoList) {
                final RouteLineInfo routeResult = new RouteLineInfo();
                routeResult.setMPathID(info.getPathID());
                routeResult.setMType(info.getType());
                routeResult.setMLength(TimeUtils.getInstance().getDistanceString(info.getLength()));
                routeResult.setMDistance(info.getLength());
                routeResult.setMTotalTime(info.getTravelTime());
                routeResult.setMTravelTime(TimeUtils.getInstance().getTimeStr(info.getTravelTime()));
                routeResult.setMStaticTravelTime(TimeUtils.getInstance().getTimeStr(info.getStaticTravelTime()));
                routeResult.setMTollCost("" + info.getTollCost());
                routeResult.setMTrafficLightCount(info.getTrafficLightCount() + "");
                routeResult.setMNaviID(info.getNaviID());
                routeResult.setMIsOnline(info.isOnline());
                routeResult.setMElecRouteBool(BevPowerCarUtils.getInstance().bevCarElicOpen);
                String elecRouteLabel = "不可达";
                boolean canBeArrive = false;
                if (onlineRoute) {
                    final ArrayList<Integer> list = info.getElecPathInfo().mEnergyConsume.vehiclechargeleft;
                    if (!list.isEmpty() && list.get(list.size() - 1) != -1) {
                        final Integer elecNum = list.get(list.size() - 1);
                        final int elec = (int) (elecNum / BevPowerCarUtils.getInstance().maxBattenergy / 1000);
                        elecRouteLabel = elec + "%可达";
                        canBeArrive = true;
                        routeResult.setMRemainPercent(elec);
                    }
                } else {
                    final int remain = (int) (BevPowerCarUtils.getInstance().initlialHVBattenergy
                            * BevPowerCarUtils.getInstance().batterToDistance - info.getLength());
                    if (remain > 0) {
                        final int elec = (int) (remain * 100 / BevPowerCarUtils.getInstance().batterToDistance
                                / BevPowerCarUtils.getInstance().maxBattenergy);
                        elecRouteLabel = elec + "%可达";
                        canBeArrive = true;
                        routeResult.setMRemainPercent(elec);
                    }
                }
                // 判断补能计划是否生效
                routeResult.setMChargingStation(info.getChargeStationInfo() != null
                        && !info.getChargeStationInfo().isEmpty());
                routeResult.setMElecRouteLabel(elecRouteLabel);
                routeResult.setMCanBeArrive(canBeArrive);
                // 算路还原无法设置电车参数所以电量显示为--，可达
                if (restoration) {
                    routeResult.setMElecRouteLabel("--");
                    routeResult.setMRestoration(true);
                }
                String label = "默认";
                if (info.getLabelInfoCount() > NumberUtils.NUM_0 && !ConvertUtils.isEmpty(info.getLabelInfo((short) NumberUtils.NUM_0).content)) {
                    label = info.getLabelInfo((short) NumberUtils.NUM_0).content;
                } else if (info.getReverseLabelInfoCount() > NumberUtils.NUM_0
                        && !ConvertUtils.isEmpty(info.getReverseLabelInfo((short) NumberUtils.NUM_0).content)) {
                    label = info.getReverseLabelInfo((short) NumberUtils.NUM_0).content;
                }
                routeResult.setMLabel(label);
//                routeResult.setMRouteLineSegmentInfos(getRouteLineDetail(info));
                routeResults.add(routeResult);
            }
        }
        return routeResults;
    }

    /**
     * 路线详情
     *
     * @param info 算路信息
     * @return 算路详情
     **/
    private List<RouteLineSegmentInfo> getRouteLineDetail(final PathInfo info) {
        final List<RouteLineSegmentInfo> routeLineSegmentInfoParentList = new ArrayList<>();
        final long groupSegmentCount = info.getGroupSegmentCount();
        if (groupSegmentCount > NumberUtils.NUM_0) {
            for (int t = NumberUtils.NUM_0; t < groupSegmentCount; t++) {
                final RouteLineSegmentInfo routeLineSegmentInfoParent = new RouteLineSegmentInfo();
                //聚合段 -- 道路名
                String parentRoadName = "无名路";
                final String roadName = info.getGroupSegment(t).roadName;
                if (!ConvertUtils.isEmpty(roadName)) {
                    parentRoadName = roadName;
                }
                routeLineSegmentInfoParent.setMLoadName(parentRoadName);
                //聚合 -- 道路距离
                final String parentRoadLength = TimeUtils.getInstance().getDistanceString(info.getGroupSegment(t).length);
                routeLineSegmentInfoParent.setMDistance(parentRoadLength);

                //聚合段 -- 开始索引
                final int startSegmentIndex = info.getGroupSegment(t).startSegmentIndex;

                //聚合段 -- 转向ID
                int parentTypeID;
                parentTypeID = info.getSegmentInfo(startSegmentIndex).getMainAction();
                if (parentTypeID == MainAction.MainActionNULL) {
                    parentTypeID = info.getSegmentInfo(startSegmentIndex).getAssistantAction();
                    Logger.i(TAG, "AssistantAction_jh_line: " + parentTypeID);
                }
                routeLineSegmentInfoParent.setMIconType(parentTypeID);

                //聚合段 -- 其中导航段数量
                final int segmentCount = info.getGroupSegment(t).segmentCount;
                int lightCount = NumberUtils.NUM_0;
                final List<Long> avoidRoadList = new ArrayList<>();
                final List<RouteLineSegmentInfo> routeLineSegmentInfoChildList = new ArrayList<>();
                if (segmentCount > NumberUtils.NUM_0) {
                    for (int s = NumberUtils.NUM_0; s < segmentCount; s++) {
                        final int currentSegmentIndex = s + startSegmentIndex;
                        lightCount += (int) info.getSegmentInfo(currentSegmentIndex).getTrafficLightNum();
                        avoidRoadList.addAll(getAvoidRoadList(info.getSegmentInfo(currentSegmentIndex)));
                        final RouteLineSegmentInfo routeLineSegmentInfoChild = new RouteLineSegmentInfo();
                        String childRoadName = "无名路";
                        if (currentSegmentIndex < info.getSegmentCount() - NumberUtils.NUM_1 && !ConvertUtils
                                .isEmpty(info.getSegmentInfo(currentSegmentIndex + NumberUtils.NUM_1).getLinkInfo(NumberUtils.NUM_0).getRoadName())) {
                            childRoadName = info.getSegmentInfo(currentSegmentIndex + NumberUtils.NUM_1).getLinkInfo(NumberUtils.NUM_0).getRoadName();
                        }

                        if (currentSegmentIndex == info.getSegmentCount() - NumberUtils.NUM_1) {
                            childRoadName = "终点";
                        }
                        //导航段 -- 下个路名
                        routeLineSegmentInfoChild.setMLoadName(childRoadName);
                        //导航段 -- 当前红绿灯
                        routeLineSegmentInfoChild.setMLightCount(lightCount);
                        //导航段 -- 距离
                        final String childLength = TimeUtils.getInstance().getDistanceString(info.getSegmentInfo(currentSegmentIndex).getLength());
                        routeLineSegmentInfoChild.setMDistance(childLength);
                        //导航段 -- 转向ID
                        int childTypeID = info.getSegmentInfo(currentSegmentIndex).getMainAction();
                        if (childTypeID == MainAction.MainActionNULL) {
                            childTypeID = info.getSegmentInfo(currentSegmentIndex).getAssistantAction();
                            Logger.i(TAG, "AssistantAction_dh_line: " + childTypeID);
                        }
                        routeLineSegmentInfoChild.setMIconType(childTypeID);
                        routeLineSegmentInfoChildList.add(routeLineSegmentInfoChild);
                    }
                }
                //聚合段 -- 红绿灯总数
                routeLineSegmentInfoParent.setMLightCount(lightCount);
                routeLineSegmentInfoParent.setMAvoidList(avoidRoadList);

                routeLineSegmentInfoParent.setMRouteLineSegmentInfos(routeLineSegmentInfoChildList);
                routeLineSegmentInfoParentList.add(routeLineSegmentInfoParent);
            }
        }
        return routeLineSegmentInfoParentList;
    }


    /**
     * 获取规避路线集合
     *
     * @param segmentInfo 路线内导航段信息类
     * @return 算路详情
     **/
    private List<Long> getAvoidRoadList(final SegmentInfo segmentInfo) {
        final List<Long> avoidRoadList = new ArrayList<>();
        if (segmentInfo.getLinkCount() <= NumberUtils.NUM_0) {
            return avoidRoadList;
        }
        for (int t = NumberUtils.NUM_0; t < segmentInfo.getLinkCount(); t++) {
            avoidRoadList.add(segmentInfo.getLinkInfo(t).get64TopoID().longValue());
        }
        return avoidRoadList;
    }

    private final IRouteResultObserver mRouteResultObserver = new IRouteResultObserver() {
        @Override
        public void onNewRoute(final PathResultData pathResultData, final ArrayList<PathInfo> pathInfoList, final RouteLimitInfo routeLimitInfo) {
            Logger.i(TAG, "平行路切换onNewRoute pathResultData -> " + pathResultData.errorCode, " pathInfoList -> "
                    + pathInfoList.size(), " routeLimitInfo -> " + routeLimitInfo);
            if (ConvertUtils.isEmpty(pathResultData)) {
                return;
            }
            final long requestId = pathResultData.requestId;
            Logger.i(TAG, "route plane result id " + requestId);
            final RequestRouteResult requestRouteResult = ConvertUtils.containToValue(mRouteResultDataHashtable, requestId);
            Logger.i(TAG, "从请求队列中获取结果对象 " + requestRouteResult);
            if (null == requestRouteResult) {
                //除了reRoute/requestRoute外其余一概不处理
                Logger.i(TAG, "下发路线");
                return;
            }
            //专为通勤模式添加回调
            if (requestRouteResult.getMRouteRequestCallBackType() != -1) {
                handlerTMCForMap(pathInfoList, requestId, requestRouteResult.getMMapTypeId()
                        , requestRouteResult.getMRouteRequestCallBackType());
                Logger.i(TAG, "通勤模式");
                return;
            }
            mRequsetId = requestId;
            handResultSuccess(getMsgs(requestRouteResult.getMRouteWay(), false));
            handlerRouteResult(requestRouteResult, pathInfoList);
            handlerChargingStation(requestRouteResult.getMRouteChargeStationParam(), pathInfoList,
                    requestId, requestRouteResult.getMMapTypeId());
            handlerDrawLine(requestRouteResult.getMLineLayerParam(), pathInfoList, requestId,
                    requestRouteResult.getMMapTypeId(), requestRouteResult.isMIsOnlineRoute());
            handlerRouteDetailsResult(requestRouteResult, pathInfoList);
            handlerRestArea(requestRouteResult.getMRouteRestAreaParam(), pathInfoList, requestId,
                    requestRouteResult.getMMapTypeId(), requestRouteResult.isMIsOnlineRoute());
            handlerRestriction(requestRouteResult.getMRouteRestrictionParam(), pathInfoList, requestId,
                    requestRouteResult.getMMapTypeId(), requestRouteResult.isMIsOnlineRoute());
            handlerRange(pathInfoList, requestRouteResult.isMIsOnlineRoute());
            handlerCityAdCode(requestRouteResult.getMRouteAlongCityParam(), pathInfoList, requestId,
                    requestRouteResult.getMMapTypeId(), requestRouteResult.isMIsOnlineRoute());
        }

        @Override
        public void onNewRouteError(final PathResultData pathResultData, final RouteLimitInfo routeLimitInfo) {
            Logger.i(TAG, "平行路切换onNewRouteError pathResultData : ", pathResultData,
                    "routeLimitInfo : ", routeLimitInfo);
            if (ConvertUtils.isEmpty(mRouteResultObserverHashtable)) {
                return;
            }
            final RequestRouteResult requestRouteResult = ConvertUtils.containToValue(mRouteResultDataHashtable, pathResultData.requestId);
            if (!ConvertUtils.isEmpty(requestRouteResult)) {
                //专为通勤模式添加回调
                if (requestRouteResult.getMRouteRequestCallBackType() != -1) {
                    handlerTMCForMap(null, pathResultData.requestId, requestRouteResult.getMMapTypeId()
                            , requestRouteResult.getMRouteRequestCallBackType());
                    return;
                }
                final String errorMsg = getMsgs(requestRouteResult.getMRouteWay(), true);
                final String errorMsgDetail = getErrorMsgsDetails(pathResultData.errorCode);
                for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
                    if (resultObserver == null) {
                        Logger.d(TAG, "resultObserver is null ");
                        continue;
                    }
                    resultObserver.onRouteFail(requestRouteResult, pathResultData.errorCode, errorMsg + errorMsgDetail, pathResultData.requestId);
                }
            }
        }

        private String getMsgs(final RouteWayID routeWay, final boolean isError) {
            switch (routeWay) {
                case ROUTE_WAY_DEFAULT:
                    return isError ? "算路请求失败" : "";
                case ROUTE_WAY_REFRESH:
                    return isError ? "路线刷新失败" : "路线刷新成功";
                case ROUTE_WAY_AVOID:
                    return isError ? "道路避开失败" : "路线刷新成功";
                case ROUTE_WAY_ADD_VIA:
                    return isError ? "途经点添加失败" : "途经点添加成功";
                case ROUTE_WAY_DELETE_VIA:
                    return isError ? "途经点删除失败" : "途经点删除成功";
                case ROUTE_WAY_SORT_VIA:
                    return isError ? "途经点排序失败" : "途经点排序成功";
                case ROUTE_WAY_CHANGE_END:
                    return isError ? "终点更换失败" : "终点更换成功";
                default:
                    return isError ? "算路请求失败" : "";
            }
        }

        private String getErrorMsgsDetails(final int errorCode) {
            switch (errorCode) {
                case 822083597:
                case 822083601:
                case 822083607:
                case 822083608:
                    return "，无本地离线数据";
                case 822083598:
                    return "，用户取消算路";
                case 822083586:
                    return "，起点不在支持范围内";
                case 822083589:
                    return "，终点不在支持范围内";
                case 822083604:
                    return "，途径点不在支持范围内";
                case 822083646:
                case 822083653:
                    return "，起终点位置过近";
                case 822083585:
                    if (Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
                        return "，网络连接出错，请重试";
                    } else {
                        return "，无网络连接，正在为您进行离线路线规划";
                    }
                case 822083587:
                case 822083596:
                case 822083595:
                default:
                    return "，请重试";
            }
        }
    };

    /**
     * 算路请求成功回调，用以提示HMI结束Loading框和清除队列里的空对象
     *
     * @param successMsg 成功消息
     */
    private void handResultSuccess(final String successMsg) {
        for (String key : mRouteResultObserverHashtable.keySet()) {
            final RouteResultObserver resultObserver = mRouteResultObserverHashtable.get(key);
            if (resultObserver != null) {
                resultObserver.onRouteSuccess(successMsg);
            } else {
                Logger.d(TAG, "resultObserver is null ");
                mRouteResultObserverHashtable.remove(key);
            }
        }
    }

    /**
     * 封装给HMI的路段聚合信息及路线详情
     *
     * @param requestRouteResult 路线结果类
     * @param pathInfoList       路线信息
     */
    private void handlerRouteResult(final RequestRouteResult requestRouteResult, final ArrayList<PathInfo> pathInfoList) {
        final List<RouteLineInfo> routeResults = convertPathInfo2RouteResult(pathInfoList, requestRouteResult.isMIsOnlineRoute(),
                requestRouteResult.isMRestoration());
        requestRouteResult.setMRouteLineInfos(routeResults);
        Logger.i(TAG, "请求结果获取透传给HMI的信息 " + routeResults);
        for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
            if (resultObserver == null) {
                continue;
            }
            resultObserver.onRouteResult(requestRouteResult);
        }
    }

    /**
     * 封装给HMI的路段聚合信息及路线详情
     *
     * @param requestRouteResult 路线结果类
     * @param pathInfoList       路线信息
     */
    private void handlerRouteDetailsResult(final RequestRouteResult requestRouteResult, final ArrayList<PathInfo> pathInfoList) {
        List<RouteLineInfo> mRouteLineInfos = requestRouteResult.getMRouteLineInfos();
        if (!ConvertUtils.isEmpty(mRouteLineInfos) && !ConvertUtils.isEmpty(mRouteLineInfos) && pathInfoList.size() == mRouteLineInfos.size()) {
            for (int t = 0; t < pathInfoList.size(); t++) {
                mRouteLineInfos.get(t).setMRouteLineSegmentInfos(getRouteLineDetail(pathInfoList.get(t)));
            }
        }
    }

    /**
     * 路线绘制回调接口
     *
     * @param routeLineLayerParam 路线绘制参数
     * @param pathInfoList        路线信息
     * @param requestId           请求Id
     * @param mapTypeId           视图Id
     * @param onlineRoute         是否在线算路
     */
    private void handlerDrawLine(final RouteLineLayerParam routeLineLayerParam, final ArrayList<PathInfo> pathInfoList,
                                 final long requestId, final MapType mapTypeId, final boolean onlineRoute) {
        if(ConvertUtils.isEmpty(routeLineLayerParam)) return;
        routeLineLayerParam.setMRequestId(requestId);
        routeLineLayerParam.setMMapTypeId(mapTypeId);
        routeLineLayerParam.setMIsOnlineRoute(onlineRoute);
        routeLineLayerParam.setMPathInfoList(pathInfoList);
        routeLineLayerParam.setMSelectIndex(NumberUtils.NUM_0);
//        Logger.i(TAG, "获取路线绘制的参数 " + GsonUtils.toJson(routeLineLayerParam));
        for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
            if (resultObserver == null) {
                continue;
            }
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
     * @param onlineRoute        是否在线算路
     */
    private void handlerRestArea(final RouteRestAreaParam routeRestAreaParam, final ArrayList<PathInfo> pathInfoList,
                                 final long requestId, final MapType mapTypeId, final boolean onlineRoute) {
        routeRestAreaParam.setMRequestId(requestId);
        routeRestAreaParam.setMMapTypeId(mapTypeId);
        routeRestAreaParam.setMIsOnlineRoute(onlineRoute);
        routeRestAreaParam.setMPathInfoList(pathInfoList);
        final ArrayList<RouteRestAreaInfo> routeRestAreaInfos = new ArrayList<>();
        for (PathInfo pathInfo : pathInfoList) {
            final RouteRestAreaInfo routeRestAreaInfo = new RouteRestAreaInfo();
            final ArrayList<RestAreaInfo> restAreas = pathInfo.getRestAreas(0, 100);
            final List<RouteRestAreaDetailsInfo> routeRestAreaDetailsInfos = new ArrayList<>();
            for (RestAreaInfo info : restAreas) {
                final RouteRestAreaDetailsInfo routeRestAreaDetailsInfo = getRouteRestAreaDetailsInfo(info);
                routeRestAreaDetailsInfos.add(routeRestAreaDetailsInfo);
            }
            routeRestAreaInfo.setMRouteRestAreaDetailsInfos(routeRestAreaDetailsInfos);
            routeRestAreaInfos.add(routeRestAreaInfo);
        }
        routeRestAreaParam.setMRouteRestAreaInfos(routeRestAreaInfos);
        for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
            if (resultObserver == null) {
                continue;
            }
            resultObserver.onRouteRestAreaInfo(routeRestAreaParam);
        }
    }

    /**
     * 转化服务区信息
     *
     * @param info 服务区信息
     * @return 服务区信息
     */
    private RouteRestAreaDetailsInfo getRouteRestAreaDetailsInfo(final RestAreaInfo info) {
        final RouteRestAreaDetailsInfo infos = new RouteRestAreaDetailsInfo();
        infos.setMRemainDist(info.remainDist);
        infos.setMRemainTime(info.remainTime);
        infos.setMServiceName(info.serviceName);
        infos.setMServicePOIID(info.servicePOIID);
        infos.setMPos(new com.fy.navi.service.define.route.Coord2DDouble(info.pos.lon, info.pos.lat));
        infos.setMSapaDetail(info.sapaDetail);
        return infos;
    }

    /**
     * 路线上充电站数据
     *
     * @param pathInfoList 路线信息
     * @param requestId    请求Id
     * @param mapTypeId    视图Id
     * @param key          家或公司
     */
    private void handlerTMCForMap(final ArrayList<PathInfo> pathInfoList, final long requestId
            , final MapType mapTypeId, final int key) {
        final RouteTMCParam param = new RouteTMCParam();
        param.setMMapTypeId(mapTypeId);
        param.setMRequestId(requestId);
        param.setMKey(key);
        final List<RouteLightBarItem> routeLightBarItems = new ArrayList<>();
        if (!ConvertUtils.isEmpty(pathInfoList)) {
            final int allDis = (int) pathInfoList.get(0).getLength();
            param.setMTime(TimeUtils.getInstance().getTimeStr(pathInfoList.get(0).getTravelTime()));
            param.setMTimeArrive(TimeUtils.getInstance().getCurrentTimePlusSeconds(pathInfoList.get(0).getTravelTime()));
            param.setMIsShort(allDis <= 1000);
            final ArrayList<LightBarItem> lightBarItems = pathInfoList.get(0).getLightBarItems();
            int allPercent = 100;
            for (int t = 0; t < lightBarItems.size(); t++) {
                final RouteLightBarItem lightBarItem = new RouteLightBarItem();
                lightBarItem.setMStatus(lightBarItems.get(t).status);
                if (t == lightBarItems.size() - 1) {
                    if (allPercent < 0) {
                        lightBarItem.setMPercent(0);
                    } else {
                        lightBarItem.setMPercent(allPercent);
                    }
                } else {
                    lightBarItem.setMPercent(lightBarItems.get(t).length * 100 / allDis);
                }
                allPercent = allPercent - lightBarItems.get(t).length * 100 / allDis;
                routeLightBarItems.add(lightBarItem);
            }
        }
        param.setMRouteLightBarItem(routeLightBarItems);
        for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
            if (resultObserver == null) {
                continue;
            }
            resultObserver.onRouteTMCInfo(param);
        }
    }

    /**
     * 路线上充电站数据
     *
     * @param routeChargeStationParam 充电站信息
     * @param pathInfoList            路线信息
     * @param requestId               请求Id
     * @param mapTypeId               视图Id
     */
    private void handlerChargingStation(final RouteChargeStationParam routeChargeStationParam,
                                        final ArrayList<PathInfo> pathInfoList, final long requestId, final MapType mapTypeId) {
        routeChargeStationParam.setMRequestId(requestId);
        routeChargeStationParam.setMMapTypeId(mapTypeId);
        final ArrayList<RouteChargeStationInfo> chargeStationInfos = new ArrayList<>();
        routeChargeStationParam.setMPathInfoList(pathInfoList);
        for (PathInfo pathInfo : pathInfoList) {
            final RouteChargeStationInfo routeChargeStationInfo = new RouteChargeStationInfo();
            final ArrayList<RouteChargeStationDetailInfo> routeChargeStationDetailInfos = new ArrayList<>();
            if (pathInfo.getChargeStationInfo() == null || pathInfo.getChargeStationInfo().isEmpty()) {
                Logger.d("handlerChargingStation null");
                routeChargeStationInfo.setMRouteChargeStationDetailInfo(routeChargeStationDetailInfos);
                chargeStationInfos.add(routeChargeStationInfo);
                continue;
            }
            Logger.d("handlerChargingStation " + GsonUtils.toJson(pathInfo.getChargeStationInfo()));
            for (ChargeStationInfo chargeStationInfo : pathInfo.getChargeStationInfo()) {
                final RouteChargeStationDetailInfo routeChargeStationDetailInfo = getRouteChargeStationDetailInfo(chargeStationInfo);
                routeChargeStationDetailInfos.add(routeChargeStationDetailInfo);
            }
            routeChargeStationInfo.setMRouteChargeStationDetailInfo(routeChargeStationDetailInfos);
            chargeStationInfos.add(routeChargeStationInfo);
        }
        routeChargeStationParam.setMRouteChargeStationInfos(chargeStationInfos);
        for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
            if (resultObserver == null) {
                continue;
            }
            resultObserver.onRouteChargeStationInfo(routeChargeStationParam);
        }
    }

    /**
     * 转化充电站信息
     *
     * @param info 充电站信息
     * @return 充电站信息
     */
    private RouteChargeStationDetailInfo getRouteChargeStationDetailInfo(final ChargeStationInfo info) {
        final RouteChargeStationDetailInfo infos = new RouteChargeStationDetailInfo();
        infos.setMSegmentIdx(info.segmentIdx);
        infos.setMDirection(info.direction);
        infos.setMShow(new com.fy.navi.service.define.route
                .Coord2DDouble(ConvertUtils.transProjectionLatAndLon(info.show.lon), ConvertUtils.transProjectionLatAndLon(info.show.lat)));
        infos.setMProjective(new com.fy.navi.service.define.route
                .Coord2DDouble(ConvertUtils.transProjectionLatAndLon(info.projective.lon), ConvertUtils.transProjectionLatAndLon(info.projective.lat)));
        infos.setMPoiID(info.poiID);
        infos.setMName(info.name);
        infos.setMBrandName(info.brandName);
        infos.setMMaxPower(info.maxPower);
        infos.setMChargePercent(info.chargePercent);
        infos.setMChargeTime(info.chargeTime);
        infos.setMRemainingCapacity(info.remainingCapacity);
        infos.setMRemainingPercent(info.remainingPercent);
        infos.setMIndex(info.index);
        return infos;
    }

    /**
     * 路线上充电站数据回调
     *
     * @param routeAlternativeChargeStationResult 备选充电桩信息
     */
    private void handlerAlternativeChargingStation(final RouteAlternativeChargeStationResult routeAlternativeChargeStationResult) {
        final RouteAlterChargeStationParam routeAlterChargeStationParam = new RouteAlterChargeStationParam();
        routeAlterChargeStationParam.setMRequestId(routeAlternativeChargeStationResult.taskId);
        final ArrayList<RouteAlterChargeStationInfo> routeAlterChargeStationInfos = new ArrayList<>();
        if (!routeAlternativeChargeStationResult.alternativeChargeStationInfos.isEmpty()) {
            for (RouteAlternativeChargeStationInfo routeAlternativeChargeStationInfo
                    : routeAlternativeChargeStationResult.alternativeChargeStationInfos) {
                final RouteAlterChargeStationInfo routeAlterChargeStationInfo = getRouteAlterChargeStationInfo(routeAlternativeChargeStationInfo);
                routeAlterChargeStationInfos.add(routeAlterChargeStationInfo);
            }
        }
        routeAlterChargeStationParam.setMRouteAlterChargeStationInfos(routeAlterChargeStationInfos);
        for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
            if (resultObserver == null) {
                continue;
            }
            resultObserver.onRouteAlterChargeStationInfo(routeAlterChargeStationParam);
        }
    }

    /**
     * 转化替换充电站信息
     *
     * @param info 充电站信息
     * @return 替换充电站信息
     */
    private RouteAlterChargeStationInfo getRouteAlterChargeStationInfo(final RouteAlternativeChargeStationInfo info) {
        final RouteAlterChargeStationInfo infos = new RouteAlterChargeStationInfo();
        infos.setMPoiId(info.poiId);
        infos.setMName(info.name);
        infos.setMPos(new Coord3DDouble(info.pos.lon, info.pos.lat, info.pos.z));
        infos.setMRemainingCapacity(info.remainingCapacity);
        infos.setMRemainingPercent(info.remainingPercent);
        infos.setMChargeTime(info.chargeTime);
        infos.setMChildType(info.childType);
        infos.setMTagInfos(info.tagInfos);
        infos.setMSuperPlugInfo(new RouteChargeStationNumberInfo(info.superPlugInfo.totalNumber,
                info.superPlugInfo.minPower, info.superPlugInfo.maxPower));
        infos.setMFastPlugInfo(new RouteChargeStationNumberInfo(info.fastPlugInfo.totalNumber,
                info.fastPlugInfo.minPower, info.fastPlugInfo.maxPower));
        infos.setMSlowPlugInfo(new RouteChargeStationNumberInfo(info.slowPlugInfo.totalNumber,
                info.slowPlugInfo.minPower, info.slowPlugInfo.maxPower));
        infos.setMPriceInfo(new RouteAlterChargePriceInfo(info.priceInfo.lowestPriceValue, info.priceInfo.lowestPriceUnit));
        infos.setMDetourInfo(new RouteAlternativeChargeDetourInfo(info.detourInfo.distance, info.detourInfo.time));
        return infos;
    }

    /**
     * 路线上天气数据回调
     *
     * @param routeWeatherParam 天气总数据
     * @param weatherLabelItems 天气数据
     * @param requestId         请求Id
     * @param mapTypeId         视图Id
     */
    private void handlerWeather(final RouteWeatherParam routeWeatherParam, final ArrayList<WeatherLabelItem> weatherLabelItems,
                                final long requestId, final MapType mapTypeId) {
        if (ConvertUtils.isEmpty(weatherLabelItems) || weatherLabelItems.isEmpty()) {
            for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
                if (resultObserver == null) {
                    continue;
                }
                resultObserver.onRouteWeatherInfo(null);
            }
            return;
        }
        routeWeatherParam.setMRequestId(requestId);
        routeWeatherParam.setMMapTypeId(mapTypeId);
        routeWeatherParam.setMWeatherLabelItem(weatherLabelItems);
        final ArrayList<RouteWeatherInfo> routeWeatherInfos = new ArrayList<>();
        for (WeatherLabelItem weatherLabelItem : weatherLabelItems) {
            final RouteWeatherInfo routeWeatherInfo = getRouteWeatherInfo(weatherLabelItem);
            routeWeatherInfos.add(routeWeatherInfo);

        }
        routeWeatherParam.setMRouteWeatherInfos(routeWeatherInfos);
        for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
            if (resultObserver == null) {
                continue;
            }
            resultObserver.onRouteWeatherInfo(routeWeatherParam);
        }
    }

    /**
     * 转化天气信息
     *
     * @param info 天气信息
     * @return 天气信息
     */
    private RouteWeatherInfo getRouteWeatherInfo(final WeatherLabelItem info) {
        final RouteWeatherInfo infos = new RouteWeatherInfo();
        infos.setMPosition(new Coord3DDouble(info.mPosition.lon, info.mPosition.lat, info.mPosition.z));
        infos.setMWeatherType(info.mWeatherType);
        infos.setMWeatherID(info.mWeatherID);
        infos.setMRouteWeatherID(getRouteWeatherId(info.mWeatherID));
        infos.setMPathID(info.mPathID);
        infos.setMCityID(info.mCityID);
        infos.setMTimestamp(info.mTimestamp);
        infos.setMCityName(info.mCityName);
        infos.setMWeatherName(info.mWeatherName);
        infos.setMText(info.mText);
        infos.setMRank(info.mRank);
        infos.setMLinkId(info.mLinkId);
        infos.setMDistance(info.mDistance);
        infos.setMEta(info.mEta);
        infos.setMPlanChannelId(info.mPlanChannelId);
        return infos;
    }

    /**
     * 转化天气信息
     *
     * @param weatherId 天气id
     * @return 天气枚举
     */
    private RouteWeatherID getRouteWeatherId(final int weatherId) {
        switch (weatherId) {
            case 104:
            case 901:
                return RouteWeatherID.ROUTE_WEATHER_CLOUDY;
            case 302:
            case 303:
            case 304:
            case 1004:
            case 1005:
                return RouteWeatherID.ROUTE_WEATHER_THUNDER;
            case 101:
            case 102:
            case 103:
                return RouteWeatherID.ROUTE_WEATHER_MORE_CLOUDY;
            case 300:
            case 301:
            case 305:
            case 306:
            case 307:
            case 309:
                return RouteWeatherID.ROUTE_WEATHER_RAIN;
            case 400:
            case 401:
            case 402:
            case 403:
            case 404:
            case 405:
            case 406:
            case 407:
                return RouteWeatherID.ROUTE_WEATHER_SNOW;
            case 308:
            case 310:
            case 311:
            case 312:
                return RouteWeatherID.ROUTE_WEATHER_BIG_RAIN;
            case 200:
            case 201:
            case 202:
            case 203:
            case 204:
            case 205:
            case 206:
            case 207:
            case 208:
            case 209:
            case 210:
            case 211:
            case 212:
            case 213:
                return RouteWeatherID.ROUTE_WEATHER_WIND;
            case 500:
            case 501:
            case 502:
            case 503:
            case 504:
            case 505:
            case 506:
            case 507:
            case 508:
                return RouteWeatherID.ROUTE_WEATHER_FOG;
            case 313:
            case 1001:
            case 1002:
            case 1003:
                return RouteWeatherID.ROUTE_WEATHER_HAIL;
            default:
                return RouteWeatherID.ROUTE_WEATHER_SUNNY;
        }
    }

    /**
     * 限行提示
     *
     * @param routeRestrictionParam 限行总数据
     * @param pathInfoList          限行提示
     * @param requestId             请求Id
     * @param mapTypeId             视图Id
     * @param onlineRoute           是否在线算路
     */
    private void handlerRestriction(final RouteRestrictionParam routeRestrictionParam,
                                    final ArrayList<PathInfo> pathInfoList, final long requestId,
                                    final MapType mapTypeId, final boolean onlineRoute) {
        if (pathInfoList.size() <= NumberUtils.NUM_0 ||
                pathInfoList.get(NumberUtils.NUM_0).getRestrictionInfo().title.length() == NumberUtils.NUM_0) {
            return;
        }
        routeRestrictionParam.setMRequestId(requestId);
        routeRestrictionParam.setMMapTypeId(mapTypeId);
        routeRestrictionParam.setMIsOnlineRoute(onlineRoute);
        final List<RouteRestrictionInfo> routeRestrictionInfos = routeRestrictionParam.getMRouteRestrictionInfo();
        final List<String> ruleIds = routeRestrictionParam.getMRuleIds();
        for (PathInfo pathInfo : pathInfoList) {
            final RouteRestrictionInfo routeRestrictionInfo = getRouteRestrictionInfo(pathInfo.getRestrictionInfo());
            ruleIds.add(getRuleId(pathInfo.getRestrictionInfo().ruleIDs));
            routeRestrictionInfos.add(routeRestrictionInfo);
        }
        routeRestrictionParam.setMRouteRestrictionInfo(routeRestrictionInfos);
        for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
            if (resultObserver == null) {
                continue;
            }
            resultObserver.onRouteRestrictionInfo(routeRestrictionParam);
        }
    }

    /**
     * 转化限行消息
     *
     * @param info 限行消息
     * @return 限行消息
     */
    private RouteRestrictionInfo getRouteRestrictionInfo(final RestrictionInfo info) {
        final RouteRestrictionInfo infos = new RouteRestrictionInfo();
        infos.setMTitle(info.title);
        infos.setMDesc(info.desc);
        infos.setMTips(info.tips);
        infos.setMCityCode(info.cityCode);
        infos.setMType(info.type);
        infos.setMTitleType(info.titleType);
        infos.setMRuleIDs(info.ruleIDs);
        infos.setMTailNums(info.tailNums);
        return infos;
    }

    /**
     * 提取限行消息
     *
     * @param ruleIdList 限行消息
     * @return 限行消息文言
     */
    private String getRuleId(final ArrayList<Long> ruleIdList) {
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

    /**
     * 转化限行消息
     *
     * @param param 限行请求消息
     * @return 限行消息
     */
    private RestrictedAreaDetail getRouteReStrictedAreaDetail(final GReStrictedAreaResponseParam param) {
        final RestrictedAreaDetail routeReStrictedAreaDetail = new RestrictedAreaDetail();
        if (ConvertUtils.isEmpty(param)) {
            return routeReStrictedAreaDetail;
        }
        if (ConvertUtils.isEmpty(param.data)) {
            return routeReStrictedAreaDetail;
        }
        if (ConvertUtils.isEmpty(param.data.mDataRule)) {
            return routeReStrictedAreaDetail;
        }
        if (param.data.mDataRule.cities.size() <= NumberUtils.NUM_0) {
            return routeReStrictedAreaDetail;
        }

        routeReStrictedAreaDetail.setMTitle(param.data.mDataRule.cities.get(NumberUtils.NUM_0).title);
        routeReStrictedAreaDetail.setMCityCode(param.data.mDataRule.cities.get(NumberUtils.NUM_0).cityCode);
        routeReStrictedAreaDetail.setMCityName(param.data.mDataRule.cities.get(NumberUtils.NUM_0).cityName);
        if (param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.size() <= NumberUtils.NUM_0) {
            return routeReStrictedAreaDetail;
        }
        routeReStrictedAreaDetail.setMDesc(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).desc);
        routeReStrictedAreaDetail.setMEffect(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).effect);
        routeReStrictedAreaDetail.setMLocal(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).local);
        routeReStrictedAreaDetail.setMOtherdesc(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).otherdesc);
        routeReStrictedAreaDetail.setMPolicyname(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).policyname);
        routeReStrictedAreaDetail.setMRing(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).ring);
        routeReStrictedAreaDetail.setMRuleid(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).ruleid);
        routeReStrictedAreaDetail.setMSummary(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).summary);
        routeReStrictedAreaDetail.setMTime(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).time);
        routeReStrictedAreaDetail.setMVehicle(param.data.mDataRule.cities.get(NumberUtils.NUM_0).rules.get(NumberUtils.NUM_0).vehicle);
        return routeReStrictedAreaDetail;
    }


    /**
     * 沿途收费站
     *
     * @param routeRestTollGateParam 收费站数据
     * @param pathInfoList           路线信息
     * @param requestId              请求Id
     * @param mapTypeId              视图Id
     * @param onlineRoute            是否在线算路
     */
    private void handlerRestTollGate(final RouteRestTollGateParam routeRestTollGateParam, final ArrayList<PathInfo> pathInfoList,
                                     final long requestId, final MapType mapTypeId, final boolean onlineRoute) {
        routeRestTollGateParam.setMRequestId(requestId);
        routeRestTollGateParam.setMMapTypeId(mapTypeId);
        routeRestTollGateParam.setMIsOnlineRoute(onlineRoute);
        final ArrayList<RouteRestTollGateInfo> routeRestTollGateInfos = new ArrayList<>();
        for (PathInfo pathInfo : pathInfoList) {
            final RouteRestTollGateInfo routeRestTollGateInfo = new RouteRestTollGateInfo();
            final List<RouteRestTollGateDetailsInfo> routeRestAreaDetailsInfos = new ArrayList<>();
            final ArrayList<RestTollGateInfo> restTollGate = pathInfo.getRestTollGate(0, 100);
            for (RestTollGateInfo info : restTollGate) {
                final RouteRestTollGateDetailsInfo restTollGateInfo = getRestTollGateInfo(info);
                routeRestAreaDetailsInfos.add(restTollGateInfo);
            }
            routeRestTollGateInfo.setMRouteRestTollGateDetailsInfos(routeRestAreaDetailsInfos);
            routeRestTollGateInfos.add(routeRestTollGateInfo);
        }
        routeRestTollGateParam.setMRouteRestTollGateInfos(routeRestTollGateInfos);
        for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
            if (resultObserver == null) {
                continue;
            }
            resultObserver.onRouteRestTollGateInfo(routeRestTollGateParam);
        }
    }

    /**
     * 转化收费站信息
     *
     * @param info 收费站消息
     * @return 收费站消息
     */
    private RouteRestTollGateDetailsInfo getRestTollGateInfo(final RestTollGateInfo info) {
        final RouteRestTollGateDetailsInfo infos = new RouteRestTollGateDetailsInfo();
        infos.setMRemainDist(info.remainDist);
        infos.setMRemainTime(info.remainTime);
        infos.setMTollGateName(info.TollGateName);
        infos.setMPos(new com.fy.navi.service.define.route.Coord2DDouble(info.pos.lon, info.pos.lat));
        return infos;
    }

    /**
     * 沿途城市列表
     *
     * @param routeAlongCityParam 城市列表数据
     * @param pathInfoList        路线信息
     * @param requestId           请求Id
     * @param mapTypeId           视图Id
     * @param onlineRoute         是否在线算路
     */
    private void handlerCityAdCode(final RouteAlongCityParam routeAlongCityParam, final ArrayList<PathInfo> pathInfoList,
                                   final long requestId, final MapType mapTypeId, final boolean onlineRoute) {
        routeAlongCityParam.setMRequestId(requestId);
        routeAlongCityParam.setMMapTypeId(mapTypeId);
        routeAlongCityParam.setMIsOnlineRoute(onlineRoute);
        final ArrayList<RouteAlongCityInfo> routeAlongCityInfos = new ArrayList<>();
        for (PathInfo pathInfo : pathInfoList) {
            final RouteAlongCityInfo routeAlongCityInfo = new RouteAlongCityInfo();
            routeAlongCityInfo.setMAdCityList(pathInfo.getCityAdcodeList());
            routeAlongCityInfos.add(routeAlongCityInfo);
        }
        routeAlongCityParam.setMRouteAlongCityInfos(routeAlongCityInfos);
        for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
            if (resultObserver == null) {
                continue;
            }
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
     * @param onlineRoute               是否在线算路
     */
    private void handlerTrafficIncident(final RouteTrafficIncidentParam routeTrafficIncidentParam, final ArrayList<PathInfo> pathInfoList,
                                        final long requestId, final MapType mapTypeId, final boolean onlineRoute) {
        routeTrafficIncidentParam.setMRequestId(requestId);
        routeTrafficIncidentParam.setMMapTypeId(mapTypeId);
        routeTrafficIncidentParam.setMIsOnlineRoute(onlineRoute);
        final ArrayList<RouteTrafficIncidentInfo> routeTrafficIncidentInfos = new ArrayList<>();
        for (PathInfo pathInfo : pathInfoList) {
            final RouteTrafficIncidentInfo routeTrafficIncidentInfo = new RouteTrafficIncidentInfo();
            final ArrayList<RouteTrafficIncidentDetailsInfo> routeTrafficIncidentDetailsInfos = new ArrayList<>();
            for (int t = NumberUtils.NUM_0; t < pathInfo.getTrafficIncidentCount(false); t++) {
                final RouteTrafficIncidentDetailsInfo routeTrafficIncidentDetailsInfo = getRouteTrafficIncidentDetailsInfo(pathInfo
                        .getTrafficIncident((short) t, false));
                routeTrafficIncidentDetailsInfos.add(routeTrafficIncidentDetailsInfo);
            }
            routeTrafficIncidentInfo.setMRouteTrafficIncidentDetailsInfos(routeTrafficIncidentDetailsInfos);
            routeTrafficIncidentInfos.add(routeTrafficIncidentInfo);
        }
        routeTrafficIncidentParam.setMRouteTrafficIncidentInfos(routeTrafficIncidentInfos);
        for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
            if (resultObserver == null) {
                continue;
            }
            resultObserver.onRouteTrafficIncidentInfo(routeTrafficIncidentParam);
        }
    }

    /**
     * 转化算路交通信息
     *
     * @param info 交通消息
     * @return 交通消息
     */
    private RouteTrafficIncidentDetailsInfo getRouteTrafficIncidentDetailsInfo(final TrafficIncident info) {
        final RouteTrafficIncidentDetailsInfo infos = new RouteTrafficIncidentDetailsInfo();
        infos.setMPos(new com.fy.navi.service.define.route.Coord2DDouble(info.pos.lon, info.pos.lat));
        infos.setMTitle(info.title);
        infos.setMDesc(info.desc);
        infos.setMType(info.type);
        infos.setMPriority(info.priority);
        infos.setMCredibility(info.credibility);
        infos.setMSource(info.source);
        infos.setMID(info.ID);
        infos.setMEventType(info.eventType);
        infos.setMLayerTag(info.layerTag);
        infos.setMLayerId(info.layerId);
        infos.setMSegIndex(info.segIndex);
        infos.setMLinkIndex(info.linkIndex);
        infos.setMTitleType(info.titleType);
        infos.setMReversed(info.reversed);
        infos.setMLane(info.lane);
        infos.setMRoadClass(info.roadClass);
        return infos;
    }

    /**
     * 续航里程信息
     *
     * @param pathInfoList 路线信息
     * @param onlineRoute  是否在线导航
     */
    private void handlerRange(final ArrayList<PathInfo> pathInfoList, final boolean onlineRoute) {
        final ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos = new ArrayList<>();
        for (PathInfo pathInfo : pathInfoList) {
            final EndPointEnergyInfo endPointInfo = pathInfo.getDynamicMergeInfo().endPointInfo;
            final EvRangeOnRouteInfo evRangeOnRouteInfo = new EvRangeOnRouteInfo();
            evRangeOnRouteInfo.setMCanArrived(endPointInfo.remainCapacity != -1);
            final GeoPoint geoPoint = new GeoPoint();
            if (endPointInfo.energyEndFlag) {
                geoPoint.setLat(endPointInfo.energyEndPoint.show.lat);
                geoPoint.setLon(endPointInfo.energyEndPoint.show.lon);
                final GeoPoint endGeoPoint = new GeoPoint();
                endGeoPoint.setLat(pathInfo.getEndPoi().naviPos.lat);
                endGeoPoint.setLon(pathInfo.getEndPoi().naviPos.lon);
                //TODO 剩余电量计算剩余里程（当前坐标计算距离）
                RoutePackage.getInstance().getTravelTimeFutureIncludeChargeLeft(geoPoint, endGeoPoint).thenAccept(eTAInfo -> {
                    evRangeOnRouteInfo.setMRemainRangeDistance(eTAInfo.getDistance());
                });
            } else {
                geoPoint.setLat(pathInfo.getEndPoi().naviPos.lat);
                geoPoint.setLon(pathInfo.getEndPoi().naviPos.lon);
                evRangeOnRouteInfo.setMRemainRangeDistance(0);
            }
            evRangeOnRouteInfo.setMPos(geoPoint);
            evRangeOnRouteInfo.setMRemainCapacity(endPointInfo.remainCapacity);

            evRangeOnRouteInfos.add(evRangeOnRouteInfo);
        }
        Logger.i(TAG, "获取续航里程信息： " + evRangeOnRouteInfos);
        for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
            if (resultObserver == null) {
                continue;
            }
            resultObserver.onRouteRanges(evRangeOnRouteInfos);
        }
    }

    /**
     * 发送L2++数据
     *
     * @param routeCurrentPathParam 路线信息
     */
    public void sendL2SData(final RouteCurrentPathParam routeCurrentPathParam) {
        final PathInfo pathInfo = (PathInfo) routeCurrentPathParam.getMPathInfo();
        final RouteL2Data routeL2Data = getRouteL2Data(pathInfo);
        for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
            if (resultObserver == null) {
                continue;
            }
            resultObserver.onRouteL2Info(routeL2Data);
        }
    }

    /**
     * 转化路径信息
     *
     * @param pathInfo 路径消息
     * @return 路径信息
     */
    private RouteL2Data getRouteL2Data(final PathInfo pathInfo) {
        final RouteL2Data data = new RouteL2Data();
        data.setMEngineVersion(BevPowerCarUtils.getInstance().engineVersion);
        data.setMSdkVersion(BevPowerCarUtils.getInstance().sdkVersion);
        data.setMPathID(pathInfo.getPathID());
        final long segmentCount = pathInfo.getSegmentCount();
        int linkCnt = 0;
        int forCnt = 0;
        int startIndex = 0;
        final List<RouteL2Data.LinksDTO> linksDTOS = new ArrayList<>();
        final List<RouteL2Data.PntsDTO> pntsDTOS = new ArrayList<>();
        int firstIcon = 0;
        if (segmentCount > 0) {
            for (int segmentIndex = 0; segmentIndex < segmentCount; segmentIndex++) {
                //linkCnt
                final long linkCount = pathInfo.getSegmentInfo(segmentIndex).getLinkCount();
                linkCnt += linkCount;
                if (linkCnt > 0) {
                    for (int linkIndex = 0; linkIndex < linkCount; linkIndex++) {
                        final LinkInfo linkInfo = pathInfo.getSegmentInfo(segmentIndex).getLinkInfo(linkIndex);
                        //linksDTOS
                        final RouteL2Data.LinksDTO dto = new RouteL2Data.LinksDTO();
                        // TrafficLightsDTO
                        dto.setMLinkID((int) linkInfo.getTPID());
                        dto.setMFormway(linkInfo.getFormway());
                        dto.setMLen(linkInfo.getLength());
                        dto.setMLinktype(linkInfo.getLinkType());
                        dto.setMRoadclass(linkInfo.getRoadClass());
                        dto.setMRoadname(linkInfo.getRoadName());
                        dto.setMIsToll(linkInfo.isToll());
                        dto.setMUrid(linkInfo.getURID());
                        dto.setMAdminCode((int) linkInfo.getAdcode());
                        dto.setMHasMixFork(linkInfo.hasMixFork());
                        dto.setMHasTrafficLight(linkInfo.hasTrafficLight());
                        dto.setMHasMultiOut(linkInfo.hasMultiOut());
                        dto.setMMainAction(linkInfo.getMainAction());
                        dto.setMAssistantAction(linkInfo.getAssistantAction());
                        dto.setMHasParallel(linkInfo.hasParallelRoad());
                        dto.setMDirection((int) linkInfo.getRoadDirection());
                        dto.setMPntBegIdx(startIndex);
                        dto.setMPntCnt(linkInfo.getPoints().size());
                        dto.setMLaneNum((int) linkInfo.getLaneNum());
                        dto.setMSpeedLimit((int) linkInfo.getSpeedLimit());
                        dto.setMRoadOwnerShip(linkInfo.getOwnership());
                        startIndex += linkInfo.getPoints().size() - 1;
                        linksDTOS.add(dto);
                        final ArrayList<Coord2DInt32> points = linkInfo.getPoints();
                        if (points.size() > 1) {
                            if (forCnt != 0) {
                                points.remove(0);
                            }
                            forCnt++;
                            for (int poiIndex = 0 ;poiIndex < points.size(); poiIndex++) {
                                final RouteL2Data.PntsDTO pnt = new RouteL2Data.PntsDTO();
                                pnt.setMX(points.get(poiIndex).lon / 3600000.0);
                                pnt.setMY(points.get(poiIndex).lat / 3600000.0);
                                pntsDTOS.add(pnt);
                            }
                        }
                        firstIcon = linkInfo.getMainAction();
                    }
                }

            }
        }

        data.setMLinkCnt(linkCnt);
        data.setMLinks(linksDTOS);
        data.setMPnts(pntsDTOS);
        data.setMPntCnt(pntsDTOS.size());
        data.setMGuideGroups(getGuideGroupsDTO(pathInfo, firstIcon));
        data.setMTrafficLights(getTrafficLightsDTOS(pathInfo));
        data.setMRestTollGateInfos(getRestTollGateInfosDTOS(pathInfo));
        data.setMViaRoads(getViaRoadsDTOS(pathInfo, segmentCount));
        data.setMEndPoi(getEndPoiDTO());
        return data;
    }

    /**
     * 获取转化后数据
     *
     * @param pathInfo  路径消息
     * @param firstIcon firstIcon
     * @return 转化后数据
     */
    private List<RouteL2Data.GuideGroupsDTO> getGuideGroupsDTO(final PathInfo pathInfo, final int firstIcon) {
        final List<RouteL2Data.GuideGroupsDTO> guideGroupsDTOS = new ArrayList<>();
        final int groupSegmentCount = (int) pathInfo.getGroupSegmentCount();
        int linkIndex = 0;
        if (groupSegmentCount > 0) {
            for (int groupIndex = 0; groupIndex < groupSegmentCount; groupIndex++) {
                final GroupSegment groupSegment = pathInfo.getGroupSegment(groupIndex);
                final RouteL2Data.GuideGroupsDTO groupsDTO = new RouteL2Data.GuideGroupsDTO();
                final RouteL2Data.GuideGroupsDTO.GroupEnterCoordDTO enterCoordDTO = new RouteL2Data.GuideGroupsDTO.GroupEnterCoordDTO();
                final RouteL2Data.GuideGroupsDTO.GroupEnterCoordDTO.MapDTO mapDTO = new RouteL2Data.GuideGroupsDTO.GroupEnterCoordDTO.MapDTO();
                groupsDTO.setMGroupIconType(firstIcon);
                groupsDTO.setMGroupLen((int) groupSegment.length);
                groupsDTO.setMGroupName(groupSegment.roadName);
                final int num = groupSegment.segmentCount;
                final int index = groupSegment.startSegmentIndex;
                int lightSize = 0;
                int travelTime = 0;
                final List<RouteL2Data.GuideGroupsDTO.SegmentsDTO> segmentsDTOList = new ArrayList<>();
                if (num > 0) {
                    mapDTO.setMX(pathInfo.getSegmentInfo(index).getLinkInfo(0).getPoints().get(0).lon / 3600000.0);
                    mapDTO.setMY(pathInfo.getSegmentInfo(index).getLinkInfo(0).getPoints().get(0).lat / 3600000.0);
                    enterCoordDTO.setMMap(mapDTO);
                    groupsDTO.setMGroupEnterCoord(enterCoordDTO);
                    for (int lightIndex = 0; lightIndex < num; lightIndex++) {
                        final SegmentInfo segmentInfo = pathInfo.getSegmentInfo(index + lightIndex);
                        lightSize += segmentInfo.getTrafficLightNum();
                        final RouteL2Data.GuideGroupsDTO.SegmentsDTO dto = new RouteL2Data.GuideGroupsDTO.SegmentsDTO();
                        dto.setMCrntSegmLinkCnt((int) segmentInfo.getLinkCount());
                        dto.setMLinkBegIdx(linkIndex);
                        linkIndex = linkIndex + (int)segmentInfo.getLinkCount();
                        dto.setMDescription("行驶" + TimeUtils.getInstance().getDistanceString(segmentInfo.getLength())
                                + iconId2String(segmentInfo) + "进入" + segmentInfo.getLinkInfo(0).getRoadName());
                        dto.setMIsArriveWayPoint(groupSegment.isViaPoint || lightIndex == num -1);
                        dto.setMNavigationMainAction(segmentInfo.getMainAction());
                        dto.setMNavigationAssistAction(segmentInfo.getAssistantAction());
                        dto.setMNavigationNextRoadName(segmentInfo.getCrossingName());
                        dto.setMNavigationLen((int) segmentInfo.getLength());
                        dto.setMTrafficLightNum((int) segmentInfo.getTrafficLightNum());
                        dto.setMTravelTime((int) segmentInfo.getTravelTime());
                        travelTime += (int) segmentInfo.getTravelTime();
                        segmentsDTOList.add(dto);
                    }
                }
                groupsDTO.setMGroupTrafficLightsCount(lightSize);
                groupsDTO.setMGroupTime(travelTime);
                groupsDTO.setMSegments(segmentsDTOList);
                guideGroupsDTOS.add(groupsDTO);
            }
        }
        return guideGroupsDTOS;
    }

    /**
     * 获取转化后数据
     *
     * @param pathInfo 路径消息
     * @return 转化后数据
     */
    private List<RouteL2Data.TrafficLightsDTO> getTrafficLightsDTOS(final PathInfo pathInfo) {
        final ArrayList<Coord2DDouble> allTrafficLights = pathInfo.getAllTrafficLights();
        final List<RouteL2Data.TrafficLightsDTO> trafficLightsDTOS = new ArrayList<>();
        if (allTrafficLights.size() > 0) {
            for (Coord2DDouble coord2DDouble : allTrafficLights) {
                final RouteL2Data.TrafficLightsDTO dto = new RouteL2Data.TrafficLightsDTO();
                dto.setMX(coord2DDouble.lon);
                dto.setMY(coord2DDouble.lat);
                trafficLightsDTOS.add(dto);
            }
        }
        return trafficLightsDTOS;
    }

    /**
     * 获取转化后数据
     *
     * @param pathInfo 路径消息
     * @return 转化后数据
     */
    private List<RouteL2Data.RestTollGateInfosDTO> getRestTollGateInfosDTOS(final PathInfo pathInfo) {
        final ArrayList<RestTollGateInfo> restTollGate = pathInfo.getRestTollGate(0, 20);
        final List<RouteL2Data.RestTollGateInfosDTO> restTollGateInfosDTOS = new ArrayList<>();
        if (restTollGate.size() > 0) {
            for (RestTollGateInfo info : restTollGate) {
                final RouteL2Data.RestTollGateInfosDTO dto = new RouteL2Data.RestTollGateInfosDTO();
                dto.setMRemainDist((int) info.remainDist);
                dto.setMRemainTime((int) info.remainTime);
                dto.setMTollGateName(info.TollGateName);
                final RouteL2Data.RestTollGateInfosDTO.PosDTO pos = new RouteL2Data.RestTollGateInfosDTO.PosDTO();
                pos.setMX(info.pos.lon);
                pos.setMY(info.pos.lat);
                dto.setMPos(pos);
                restTollGateInfosDTOS.add(dto);
            }
        }
        return restTollGateInfosDTOS;
    }

    /**
     * 获取转化后数据
     *
     * @param pathInfo     路径消息
     * @param segmentCount segmentCount
     * @return 转化后数据
     */
    private List<RouteL2Data.ViaRoadsDTO> getViaRoadsDTOS(final PathInfo pathInfo, final long segmentCount) {
        final List<RouteL2Data.ViaRoadsDTO> viaRoadsDTOS = new ArrayList<>();
        int maxLane = 0;
        int minLane = 0;
        int maxSpeed = 0;
        int minSpeed = 0;
        int roadClass = 0;
        if (segmentCount > 0) {
            for (int segmentIndex = 0; segmentIndex < segmentCount; segmentIndex++) {
                final GroupSegment groupSegment = pathInfo.getGroupSegment(segmentIndex);
                if (groupSegment.isViaPoint) {
                    final RouteL2Data.ViaRoadsDTO dto = new RouteL2Data.ViaRoadsDTO();
                    dto.setMRoadName(groupSegment.roadName);
                    dto.setMLength((int) groupSegment.length);
                    final int startIndex = groupSegment.startSegmentIndex;
                    final int count = groupSegment.segmentCount;
                    if (count > 0) {
                        final RouteL2Data.ViaRoadsDTO.CoordinateDTO coordinateDTO = new RouteL2Data.ViaRoadsDTO.CoordinateDTO();
                        roadClass = pathInfo.getSegmentInfo(startIndex).getLinkInfo(0).getRoadClass();
                        dto.setMRoadClass(roadClass);
                        final Coord2DInt32 coord2DInt32 = pathInfo.getSegmentInfo(startIndex).getPoints().get(0);
                        coordinateDTO.setMX(coord2DInt32.lon / 3600000.0);
                        coordinateDTO.setMY(coord2DInt32.lat / 3600000.0);
                        dto.setMCoordinate(coordinateDTO);
                        for (int t = 0; t < count; t++) {
                            final SegmentInfo segmentInfo = pathInfo.getSegmentInfo(startIndex + t);
                            if (segmentInfo.getLinkInfo(0) == null) {
                                continue;
                            }
                            final short speed = segmentInfo.getLinkInfo(0).getSpeed();
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
                            final short laneNum = segmentInfo.getLinkInfo(0).getLaneNum();
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
                    dto.setMMinSpeedLimit(minSpeed);
                    dto.setMMaxSpeedLimit(maxSpeed);
                    dto.setMMinLaneNum(minLane);
                    dto.setMMaxLaneNum(maxLane);
                    viaRoadsDTOS.add(dto);
                }
            }
        }
        return viaRoadsDTOS;
    }

    /**
     * 获取转化后数据
     *
     * @return 转化后数据
     */
    private RouteL2Data.EndPoiDTO getEndPoiDTO() {
        final RouteL2Data.EndPoiDTO endPoiDTO = new RouteL2Data.EndPoiDTO();
        endPoiDTO.setMId(mPoiInfoEntityEnd.getPid());
        endPoiDTO.setMName(mPoiInfoEntityEnd.getName());
        endPoiDTO.setMType(mPoiInfoEntityEnd.getPoiType());
        if (!ConvertUtils.isEmpty(mPoiInfoEntityEnd)) {
            final List<RouteL2Data.EndPoiDTO.EntranceListDTO> entranceListDTOS = new ArrayList<>();
            final List<RouteL2Data.EndPoiDTO.ExitListDTO> exitListDTOS = new ArrayList<>();
            final List<RouteL2Data.EndPoiDTO.ParkingInfoListDTO> parkingInfoListDTOS = new ArrayList<>();
            if (!ConvertUtils.isEmpty(mPoiInfoEntityEnd) && !ConvertUtils.isEmpty(mPoiInfoEntityEnd.getParkingInfoList())
                    && mPoiInfoEntityEnd.getParkingInfoList().size() > 0) {
                for (ParkingInfo info : mPoiInfoEntityEnd.getParkingInfoList()) {
                    final RouteL2Data.EndPoiDTO.ParkingInfoListDTO parkingInfoListDTO = new RouteL2Data.EndPoiDTO.ParkingInfoListDTO();
                    parkingInfoListDTO.setMId(info.getPoiId());
                    parkingInfoListDTO.setMName(info.getName());
                    parkingInfoListDTO.setMType(info.getQueryType());
                    final List<RouteL2Data.EndPoiDTO.ParkingInfoListDTO.EntranceListDTO> entranceListDTOArrayList = new ArrayList<>();
                    final List<RouteL2Data.EndPoiDTO.ParkingInfoListDTO.ExitListDTO> exitListDTOArrayList = new ArrayList<>();
                    if (!ConvertUtils.isEmpty(info) && !ConvertUtils.isEmpty(info.getSearchParkInOutInfos())
                            && !info.getSearchParkInOutInfos().isEmpty()) {
                        for (SearchParkInOutInfo searchParkInOutInfo : info.getSearchParkInOutInfos()) {
                            final RouteL2Data.EndPoiDTO.EntranceListDTO dto = new RouteL2Data.EndPoiDTO.EntranceListDTO();
                            final RouteL2Data.EndPoiDTO.ExitListDTO exitListDTO = new RouteL2Data.EndPoiDTO.ExitListDTO();
                            if (searchParkInOutInfo.getEntExitId().equals("出口")) {
                                exitListDTO.setMX(searchParkInOutInfo.getX());
                                exitListDTO.setMY(searchParkInOutInfo.getY());
                                exitListDTOS.add(exitListDTO);
                            } else if (searchParkInOutInfo.getEntExitId().equals("入口")){
                                dto.setMX(searchParkInOutInfo.getX());
                                dto.setMY(searchParkInOutInfo.getY());
                                entranceListDTOS.add(dto);
                            } else {
                                dto.setMX(searchParkInOutInfo.getX());
                                dto.setMY(searchParkInOutInfo.getY());
                                exitListDTO.setMX(searchParkInOutInfo.getX());
                                exitListDTO.setMY(searchParkInOutInfo.getY());
                                entranceListDTOS.add(dto);
                                exitListDTOS.add(exitListDTO);
                            }
                            final RouteL2Data.EndPoiDTO.ParkingInfoListDTO.EntranceListDTO entranceListDTO
                                    = new RouteL2Data.EndPoiDTO.ParkingInfoListDTO.EntranceListDTO();
                            final RouteL2Data.EndPoiDTO.ParkingInfoListDTO.ExitListDTO exitListDTOBean
                                    = new RouteL2Data.EndPoiDTO.ParkingInfoListDTO.ExitListDTO();
                            if (searchParkInOutInfo.getEntExitId().equals("出口")) {
                                exitListDTOBean.setMX(searchParkInOutInfo.getX());
                                exitListDTOBean.setMY(searchParkInOutInfo.getY());
                                exitListDTOArrayList.add(exitListDTOBean);
                            } else if (searchParkInOutInfo.getEntExitId().equals("入口")){
                                entranceListDTO.setMX(searchParkInOutInfo.getX());
                                entranceListDTO.setMY(searchParkInOutInfo.getY());
                            } else {
                                entranceListDTO.setMX(searchParkInOutInfo.getX());
                                entranceListDTO.setMY(searchParkInOutInfo.getY());
                                exitListDTOBean.setMX(searchParkInOutInfo.getX());
                                exitListDTOBean.setMY(searchParkInOutInfo.getY());
                                entranceListDTOArrayList.add(entranceListDTO);
                                exitListDTOArrayList.add(exitListDTOBean);
                            }
                        }
                    }
                    parkingInfoListDTO.setMEntranceList(entranceListDTOArrayList);
                    parkingInfoListDTO.setMExitList(exitListDTOArrayList);
                    parkingInfoListDTOS.add(parkingInfoListDTO);
                }
            }
        }
        return endPoiDTO;
    }

    /**
     * 转换文言
     *
     * @param segmentInfo 导航段信息
     * @return 转化后的文言
     */
    private String iconId2String(final SegmentInfo segmentInfo) {
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
            default:
                return "直行";

        }
    }

    private final IRouteWeatherObserver mRouteWeatherObserver = (requestId, arrayList) -> {
        Logger.i(TAG, "requestId -> " + requestId, "arrayList -> " + arrayList);
        final RequestRouteResult requestRouteResult = ConvertUtils.containToValue(mRouteResultDataHashtable, mRequsetId);
        if (requestRouteResult == null) {
            Logger.e(TAG, "have no this data");
            return;
        }
        handlerWeather(requestRouteResult.getMRouteWeatherParam(), arrayList, requestId, requestRouteResult.getMMapTypeId());
    };

    private final IRouteAlternativeChargeStationObserver mRouteAlternativeChargeStationObserver = routeAlternativeChargeStationResult -> {
        Logger.i(TAG, " requestId -> " + GsonUtils.toJson(routeAlternativeChargeStationResult));
        handlerAlternativeChargingStation(routeAlternativeChargeStationResult);
    };

    public IRouteAlternativeChargeStationObserver getRouteAlternativeChargeStationObserver() {
        return mRouteAlternativeChargeStationObserver;
    }

    private final INaviRerouteObserver mRerouteObserver = new INaviRerouteObserver() {
        @Override
        public void onModifyRerouteOption(RouteOption rerouteOption) {
            Logger.d(TAG, "平行路切换onModifyRerouteOption onReroute : " + rerouteOption.getRouteReqId());
            if (OpenApiHelper.powerType() == PowerType.E_VEHICLE_ENERGY_ELECTRIC) {
                float powerLeft = SignalPackage.getInstance().getBatteryEnergy();
                Logger.i(TAG, "纯电汽车 需要传剩余电量数据 powerLeft：" + powerLeft);
                rerouteOption.setVehicleCharge(powerLeft);
            }
        }

        @Override
        public void onRerouteInfo(BLRerouteRequestInfo info) {
            Logger.i(TAG, "平行路切换onRerouteInfo: ", info.errCode + "----" + info.requestId + "----" + info.option.getRouteType() + "----" + info.option.getRouteReqId());
            if (mRequsetId == -1 || info.requestId == 0 || ConvertUtils.isEmpty(mRouteResultDataHashtable)) {
                Logger.e(TAG, "have no this data");
                return;
            }
            final RequestRouteResult requestRouteResult = ConvertUtils.containToValue(mRouteResultDataHashtable, mRequsetId);
            if (ConvertUtils.isEmpty(requestRouteResult)) {
                Log.e(TAG, "onRerouteInfo: 请求参数已经被清空");
                return;
            }
            requestRouteResult.setMRouteWay(RouteWayID.ROUTE_WAY_DEFAULT);
            requestRouteResult.setMFastNavi(true);
            if (info.option.getRouteType() == RouteType.RouteTypeYaw) {
                Logger.i(TAG, "onReroute: 偏航引发的重算");
                mRouteResultDataHashtable.put(info.requestId, requestRouteResult);
                for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
                    if (resultObserver == null) {
                        continue;
                    }
                    resultObserver.onReRoute();
                }
            } else if (info.option.getRouteType() == RouteType.RouteTypeTMC) {
                Logger.i(TAG, "onReroute: TMC引发的重算");
                mRouteResultDataHashtable.put(info.requestId, requestRouteResult);
                for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
                    if (resultObserver == null) {
                        continue;
                    }
                    resultObserver.onReRoute();
                }
            } else {
                Logger.i(TAG, "onReroute: 其他情况引发的重算");
                mRouteResultDataHashtable.put(info.requestId, requestRouteResult);
                //其他回调速度快，用户感知不到，不展示加载框
//                for (RouteResultObserver resultObserver : mRouteResultObserverHashtable.values()) {
//                    if (resultObserver == null) {
//                        continue;
//                    }
//                    resultObserver.onReRoute();
//                }
            }
        }

        @Override
        public void onSwitchParallelRoadRerouteInfo(BLRerouteRequestInfo info) {
            //接收平行路切换自动重算的算路请求信息
            Logger.i(TAG, "onReroute:平行路切换自动重算 errCode:" + info.errCode + " requestId:" + info.requestId);
        }
    };

    /**
     * 设置终点信息
     *
     * @param poiInfoEntity 点消息
     */
    public void sendEndEntity(final PoiInfoEntity poiInfoEntity) {
        mPoiInfoEntityEnd = poiInfoEntity;
    }
}