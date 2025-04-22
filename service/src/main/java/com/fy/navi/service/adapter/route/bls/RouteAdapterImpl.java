package com.fy.navi.service.adapter.route.bls;

import static com.autonavi.gbl.common.path.model.PointType.PointTypeStart;
import static com.autonavi.gbl.common.path.option.ParalleType.paralleTypeMainSide;
import static com.autonavi.gbl.common.path.option.ParalleType.paralleTypeOverhead;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.aosclient.BLAosService;
import com.autonavi.gbl.common.model.ElecCommonParameter;
import com.autonavi.gbl.common.model.ElecCostList;
import com.autonavi.gbl.common.model.ElecInfoConfig;
import com.autonavi.gbl.common.model.ElecSpeedCostList;
import com.autonavi.gbl.common.model.PowertrainLoss;
import com.autonavi.gbl.common.path.model.POIInfo;
import com.autonavi.gbl.common.path.model.PointType;
import com.autonavi.gbl.common.path.option.CurrentPositionInfo;
import com.autonavi.gbl.common.path.option.POIForRequest;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.common.path.option.RouteOption;
import com.autonavi.gbl.common.path.option.RouteType;
import com.autonavi.gbl.pos.model.LocSwitchRoadType;
import com.autonavi.gbl.route.RouteService;
import com.autonavi.gbl.route.model.RouteAlternativeChargeStationParam;
import com.autonavi.gbl.route.model.RouteChargingPreference;
import com.autonavi.gbl.route.model.RouteControlKey;
import com.autonavi.gbl.route.model.RouteRestorationOption;
import com.autonavi.gbl.scene.SceneModuleService;
import com.autonavi.gbl.scene.model.InitSceneModuleParam;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.user.msgpush.model.AimRoutePushMsg;
import com.autonavi.gbl.user.msgpush.model.MobileRouteParam;
import com.autonavi.gbl.user.msgpush.model.MobileVehicleInfo;
import com.autonavi.gbl.user.msgpush.model.RoutepathrestorationPathInfo;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.autonavi.gbl.util.model.TaskResult;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.route.IRouteApi;
import com.fy.navi.service.adapter.route.RouteResultObserver;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteAvoidInfo;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.route.RouteRequestParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.utils.BevPowerCarUtils;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

/**
 * 算路服务.
 * @author lvww
 * @version  \$Revision.1.0\$
 * description Impl类只做SDK的原子能力封装，不做对象及数据转换
 * date 2024/12/5
 */
public class RouteAdapterImpl implements IRouteApi {
    private static final String TAG = MapDefaultFinalTag.ROUTE_SERVICE_TAG;
    private static final String NUM_ONE = "1";
    private RouteService mRouteService;
    private RouteAdapterImplHelper mAdapterImplHelper;
    private RouteOption mLastRouteOption;
    private RequestRouteResult mLastRequestRouteResult;
    private long homeRequestId = -1;
    private long officeRequestId = -1;
    private long mRequestRouteId = -1;

    public RouteAdapterImpl() {
        mRouteService = (RouteService) ServiceMgr.getServiceMgrInstance()
                .getBLService(SingleServiceID.RouteSingleServiceID);
        final SceneModuleService sceneModuleService = (SceneModuleService) ServiceMgr
                .getServiceMgrInstance().getBLService(SingleServiceID.SceneModuleSingleServiceID);
        // 统一初始化场景组件服务
        final InitSceneModuleParam param = new InitSceneModuleParam();
        param.bEnableDynamicCloudShowInfoModule = true; // 开启动态运营组件
        if (sceneModuleService != null) {
            sceneModuleService.init(param);
        }
        mAdapterImplHelper = new RouteAdapterImplHelper(mRouteService, new BLAosService());
    }

    @Override
    public void initRouteService() {
        mAdapterImplHelper.initRouteService();
    }

    @Override
    public void registerRouteObserver(final String key, final RouteResultObserver routeResultObserver) {
        mAdapterImplHelper.registerRouteObserver(key, routeResultObserver);
    }

    @Override
    public void setRoutePreference(final RoutePreferenceID routePreferenceID) {

    }

    @Override
    public long requestRoute(final RouteRequestParam param, final List<RouteParam> paramList) {
        mAdapterImplHelper.checkoutRouteServer();
        final RequestRouteResult requestRouteResult = new RequestRouteResult();
        requestRouteResult.setMMapTypeId(param.getMMapTypeId());
        requestRouteResult.setMFastNavi(param.isMFastNavi());
        requestRouteResult.setMRouteWay(param.getMRouteWay());
        requestRouteResult.setMIsOnlineRoute(param.isMIsOnline());
        requestRouteResult.setMRouteType(param.getMRoutePriorityType());
        requestRouteResult.setMRouteRequestCallBackType(param.getRouteRequestCallBackType());
        final RouteOption routeOption = mAdapterImplHelper.getRequestParam(requestRouteResult, paramList);
        mLastRouteOption = routeOption;
        mLastRequestRouteResult = requestRouteResult;
        //设置共性参数 比如在线优先 ....
        //记录请求ID和屏幕ID以供返回结果使用
        final long requestId = mRouteService.requestRoute(routeOption);
        mRequestRouteId = requestId;
        final Hashtable<Long, RequestRouteResult> routeResultHashtable = mAdapterImplHelper.getRouteResultDataHashtable();
        routeResultHashtable.put(requestId, requestRouteResult);
        Logger.i(TAG, "route plane request id " + requestId);
        return requestId;
    }

    @Override
    public long requestRouteWeather(final RouteLineLayerParam routeLineLayerParam, final int index) {
        if (ConvertUtils.isEmpty(mRouteService) || index == -1 || ConvertUtils.isEmpty(routeLineLayerParam)
            || ConvertUtils.isEmpty(routeLineLayerParam.getMPathInfoList())
            || routeLineLayerParam.getMPathInfoList().size() < index) {
            Logger.e(TAG, "天气请求失败");
            return -1;
        }
        return mRouteService.requestPathWeather((PathInfo) routeLineLayerParam.getMPathInfoList().get(index));
    }

    @Override
    public long requestRouteAlternativeChargeStation(final Object pathInfo, final String poiId) {
        if (ConvertUtils.isEmpty(mRouteService)) {
            return -1;
        }
        final RouteAlternativeChargeStationParam param = new RouteAlternativeChargeStationParam();
        param.poiId = poiId;
        final TaskResult taskResult = mRouteService.request((PathInfo) pathInfo, param,
                mAdapterImplHelper.getRouteAlternativeChargeStationObserver());
        Logger.d("requestRouteAlternativeChargeStation" + taskResult.errorCode);
        if (taskResult.errorCode != 0) {
            return -1;
        }
        return taskResult.taskId;
    }

    @Override
    public boolean cancelRoute(final long requestId) {
        return mRouteService.abortRequest(requestId);
    }

    @Override
    public void removeRouteObserver(final String key) {
        mAdapterImplHelper.removeRouteObserver(key);
    }

    @Override
    public void unInitRouteService() {
        mAdapterImplHelper.removeAllObserver();
        if (ServiceInitStatus.ServiceInitDone != mRouteService.isInit()) {
            mRouteService.unInit();
        }
    }
    @Override
    public void setAvoidRoad(final RouteAvoidInfo routeAvoidInfo) {
        mAdapterImplHelper.setAvoidRoad(routeAvoidInfo);
    }

    @Override
    public void setRequestControl(final RoutePreferenceID id, final String num, final boolean restriction, final boolean routePlan) {
        mAdapterImplHelper.setRoutePreference(id);

        mRouteService.control(RouteControlKey.RouteControlKeySetInvoker, routePlan ? "navi" : "plan");

        mRouteService.control(RouteControlKey.RouteControlKeyVehicleID, num);
        //设置eta请求的躲避车辆限行,0表示关闭eta限行请求，1 表示打开eta限行请求
        mRouteService.control(RouteControlKey.RouteControlKeyETARestriction, restriction ? NUM_ONE : "0");
        mRouteService.control(RouteControlKey.RouteControlKeySetTotalTime, "15000");

        mRouteService.control(RouteControlKey.RouteControlConfigSetTipsInfo, NUM_ONE);
        mRouteService.control(RouteControlKey.RouteControlKeyPrivacy, NUM_ONE);


        mRouteService.control(RouteControlKey.RouteControlKeyVehicleType, BevPowerCarUtils.getInstance().carType);
        mRouteService.control(RouteControlKey.RouteControlKeepChargingStation, BevPowerCarUtils.getInstance().bevCarElicOpen ? NUM_ONE : "0");
        if (BevPowerCarUtils.getInstance().bevCarElicOpen) {
            final RouteChargingPreference chargingPreference = new RouteChargingPreference();
            chargingPreference.brands.addAll(mRouteService.getSupportedChargingPreference().brands);
            mRouteService.setChargingPreference(chargingPreference);
            mRouteService.setElecInfoConfig(getElecInfoConfig());
            Logger.i(TAG, GsonUtils.toJson(getElecInfoConfig()));
        }
    }

    @Override
    public void sendEndEntity(final PoiInfoEntity poiInfoEntity) {
        mAdapterImplHelper.sendEndEntity(poiInfoEntity);
    }

    @Override
    public void requestRouteRestoration(final RouteMsgPushInfo routeMsgPushInfo, final MapType mapTypeId) {
        mAdapterImplHelper.checkoutRouteServer();
        // 路线还原
        Logger.i(TAG, "requestRouteRestoration");
        final AimRoutePushMsg aimRoutePushMsg = (AimRoutePushMsg) routeMsgPushInfo.getMMsgPushInfo();
        final RouteRestorationOption routeRestorationOption = new RouteRestorationOption();
        final RoutepathrestorationPathInfo path = aimRoutePushMsg.content.path;
        routeRestorationOption.setPaths(path.paths);
        routeRestorationOption.setStartPoints(path.startPoints.points);
        routeRestorationOption.setViaPoints(path.routeViaPoints.display_points, path.routeViaPoints.path_project_points);
        routeRestorationOption.setEndPoints(path.endPoints.points);

        final MobileRouteParam routeParam = aimRoutePushMsg.content.routeParam;
        routeRestorationOption.setEndName(routeParam.destination.name);
        routeRestorationOption.setContentOption(routeParam.contentOption);

        routeRestorationOption.setRouteVer(RouteService.getRouteVersion());
        routeRestorationOption.setSdkVer(RouteService.getEngineVersion());

        final MobileVehicleInfo vehicle = routeParam.vehicle;
        routeRestorationOption.setCarType(vehicle.type);
        routeRestorationOption.setCarSize(vehicle.size);
        routeRestorationOption.setCarHeight(vehicle.height);
        routeRestorationOption.setCarWidth(vehicle.width);
        routeRestorationOption.setCarLoad(vehicle.load);
        routeRestorationOption.setCarWeight(vehicle.weight);
        routeRestorationOption.setCarAxis(vehicle.axis);
        routeRestorationOption.setCarPlate(vehicle.plate);
        routeRestorationOption.setNaviId(aimRoutePushMsg.content.naviId);

        final RequestRouteResult requestRouteResult = new RequestRouteResult();
        requestRouteResult.setMMapTypeId(mapTypeId);
        requestRouteResult.setMFastNavi(false);
        requestRouteResult.setMIsOnlineRoute(true);
        requestRouteResult.setMRouteWay(RouteWayID.ROUTE_WAY_DEFAULT);
        final RouteLineLayerParam routeLineLayerParam = new RouteLineLayerParam();
        routeLineLayerParam.getMRouteLinePoints().getMEndPoints().add(routeMsgPushInfo.getMEndPoint());
        routeLineLayerParam.getMRouteLinePoints().getMStartPoints().add(routeMsgPushInfo.getMStartPoint());
        routeLineLayerParam.getMRouteLinePoints().setMViaPoints(routeMsgPushInfo.getMViaPoints());
        requestRouteResult.setMLineLayerParam(routeLineLayerParam);
        final long requestId =  mRouteService.requestRouteRestoration(routeRestorationOption);
        final Hashtable<Long, RequestRouteResult> routeResultHashtable =  mAdapterImplHelper.getRouteResultDataHashtable();
        routeResultHashtable.put(requestId, requestRouteResult);
        Logger.i(TAG, "route plane request id " + requestId);
    }

    @Override
    public void abortRequest(final long requestId) {
        final boolean success = mRouteService.abortRequest(requestId);
        Logger.i(TAG, "abortRequest: " + success);
    }

    /**
     * 获取电动车能耗参数
     * @return 电动车能耗参数
     */
    private ElecInfoConfig getElecInfoConfig() {
        //设置电动车能耗参数
        final ElecInfoConfig elecConfig = new ElecInfoConfig();
        elecConfig.orgaName = BevPowerCarUtils.getInstance().extraBrand; //车厂名称，高德商业产品定义分配
        elecConfig.vehicleConfiguration = BevPowerCarUtils.getInstance().vehicleType; //车型，应具备可读性，代表一款车型，保证一款车型只使用一个代号
        elecConfig.fesMode = BevPowerCarUtils.getInstance().curDriveMode; //驾驶模式，设置为舒适模式
        elecConfig.costModelSwitch = 0x8; //代价模型的组合开关，应用“自学习能耗模型”只需要开启“0x8”附加能耗代价开关来保证预测精度，其他项正常可不开启
        elecConfig.costUnit = BevPowerCarUtils.getInstance().energyUnit; //能量单位，设置为KWH
        elecConfig.vehiclelMass = BevPowerCarUtils.getInstance().vehicleWeight; //车重，建议使用“实时车重”提高预测精度，使用优先级：实时车重 > 半载车重 > 空载车重
        elecConfig.maxVechicleCharge = BevPowerCarUtils.getInstance().maxBattenergy; //电动汽车的最大电量负荷，即电池容量
        elecConfig.powerflag = 0x03; //eta包含充电时间+下发充电站充电时间
        elecConfig.arrivingPercent = BevPowerCarUtils.getInstance().arrivingPercent; //注意arrivingPercent和leavingPercent字面含义和真实作用相反，请勿填反
        elecConfig.leavingPercent = BevPowerCarUtils.getInstance().leavingPercent;
        elecConfig.temperature = BevPowerCarUtils.getInstance().temperature; //车辆实时外界温度
        elecConfig.vehicleCharge = BevPowerCarUtils.getInstance().initlialHVBattenergy;
        elecConfig.isCharging = BevPowerCarUtils.getInstance().charging; //当前充电状态，设置为未充电
        elecConfig.chargingPower = BevPowerCarUtils.getInstance().chargingPower; //功率

        //设置代价模型的权值
        final ElecCostList costList = new ElecCostList();
        costList.auxValue = BevPowerCarUtils.getInstance().airConditioningOpen ? 1.0f : 0.5f; //空调...
        costList.ferryRate = BevPowerCarUtils.getInstance().ferryRate;
        final ArrayList<ElecSpeedCostList> elecSpeedCostLists = new ArrayList<>();
        for (int t = 0; t < BevPowerCarUtils.getInstance().elecSpeedCostLists.size(); t++) {
            final ElecSpeedCostList elecSpeedCostList = GsonUtils.convertToT(BevPowerCarUtils.getInstance()
                    .elecSpeedCostLists.get(t),ElecSpeedCostList.class);
            elecSpeedCostLists.add(elecSpeedCostList);
        }
        costList.speedCost = elecSpeedCostLists;
        final ArrayList<PowertrainLoss> powertrainLossList = new ArrayList<>();
        for (int t = 0; t < BevPowerCarUtils.getInstance().powertrainLoss.size(); t++) {
            final PowertrainLoss powertrainLoss = GsonUtils.convertToT(BevPowerCarUtils.getInstance()
                    .powertrainLoss.get(t),PowertrainLoss.class);
            powertrainLossList.add(powertrainLoss);
        }
        costList.powertrainLoss = powertrainLossList;
        final ElecCommonParameter trans = new ElecCommonParameter(BevPowerCarUtils.getInstance()
                .trans.access, BevPowerCarUtils.getInstance().trans.decess);
        costList.trans = trans;
        final ElecCommonParameter curve = new ElecCommonParameter(BevPowerCarUtils.getInstance()
                .curve.access, BevPowerCarUtils.getInstance().curve.decess);
        costList.curve = curve;
        final ElecCommonParameter slope = new ElecCommonParameter(BevPowerCarUtils.getInstance()
                .slope.access, BevPowerCarUtils.getInstance().slope.decess);
        costList.slope = slope;
        elecConfig.costList.add(costList);
        return elecConfig;
    }

    /**
     * 平行路切换完成重算路
     * @param switchRoadType 切换类型
     * @param locInfoBean 定位
     * @param roadID 路线ID
     * @param flag 主辅路状态
     * @param hwFlag 高架桥状态
     */
    public long requestSwitchParallelRoute(int switchRoadType,LocInfoBean locInfoBean, BigInteger roadID, short flag, short hwFlag) {
        if(mLastRouteOption == null || mLastRequestRouteResult == null){
            Logger.i(TAG, "平行路切换requestSwitchParallelRoute: mLastRouteOption == null");
            return -1;
        }
        if(locInfoBean == null){
            Logger.i(TAG, "平行路切换requestSwitchParallelRoute: locInfoBean == null");
            return -1;
        }
        if(mRouteService == null){
            Logger.i(TAG, "平行路切换requestSwitchParallelRoute: mRouteService == null");
            return -1;
        }
        if(mRequestRouteId != -1){
            mRouteService.abortRequest(mRequestRouteId);
        }
        mLastRouteOption.setRouteType(RouteType.RouteTypeParallelRoad);

        // 设置行程点信息poiForRequest
        // 对于平行路切换来说终点和途径点一般保持上一次的不变，但是起点需要重新设置，且必须设置起点的道路ID
        // 起点信息poiInfo，来源于定位回调 IPosLocInfoObserver.onLocInfoUpdate
        POIForRequest poiForRequest = mLastRouteOption.getPOIForRequest();
        if (poiForRequest == null){
            Logger.i(TAG, "平行路切换requestSwitchParallelRoute: poiForRequest == null");
            return -1;
        }

        // 起点设置
        POIInfo startPoiInfo = new POIInfo();
        startPoiInfo.realPos.lon = locInfoBean.getLongitude();
        startPoiInfo.realPos.lat = locInfoBean.getLatitude();
        startPoiInfo.type = 0;//GPS点
        poiForRequest.clearPoint(PointTypeStart);
        poiForRequest.addPoint(PointTypeStart, startPoiInfo);
        POIInfo endInfo = poiForRequest.getPoint(PointType.PointTypeEnd,0);
        Logger.i(TAG, "平行路切换requestSwitchParallelRoute start lon:"+locInfoBean.getLongitude()+" lat:"+locInfoBean.getLatitude());
        if(endInfo != null){
            Logger.i(TAG, "平行路切换requestSwitchParallelRoute endInfo.name:"+endInfo.name+" lat:"+endInfo.realPos.lat+" lon:"+endInfo.realPos.lon);
        }

        poiForRequest.setDirection(locInfoBean.getMatchRoadCourse());
        poiForRequest.setReliability(locInfoBean.getCourseAcc());
        poiForRequest.setAngleType(locInfoBean.getStartDirType());
        poiForRequest.setAngleGps(locInfoBean.getGpsDir());
        poiForRequest.setAngleComp(locInfoBean.getCompassDir());
        poiForRequest.setSpeed(locInfoBean.getSpeed());
        poiForRequest.setLinkType(locInfoBean.getLinkType());
        poiForRequest.setFormWay(locInfoBean.getFormway());
        poiForRequest.setSigType(locInfoBean.getStartPosType());
        poiForRequest.setFittingDir(locInfoBean.getFittingCourse());
        poiForRequest.setMatchingDir(locInfoBean.getRoadDir());
        poiForRequest.setFittingCredit(locInfoBean.getFittingCourseAcc());
        poiForRequest.setPrecision(locInfoBean.getAccuracy());
        poiForRequest.setSourceInfo(locInfoBean.getRequestRouteInfo());

        // 平行路切换必须使用定位onParallelRoadUpdate回调的内容
        poiForRequest.setPointRoadID(PointTypeStart, 0, roadID);
        // 更新行程点信息
        mLastRouteOption.setPOIForRequest(poiForRequest);

        // 设置当前导航位置信息
        CurrentPositionInfo curLocation = new CurrentPositionInfo();
        curLocation.linkIndex = locInfoBean.getLinkCur();
        curLocation.pointIndex = locInfoBean.getPostCur();
        curLocation.segmentIndex = locInfoBean.getSegmCur();
        curLocation.overheadFlag = hwFlag; //0：无高架 1：车标在高架上（车标所在道路有对应高架下） 2：车标在高架下（车标所在道路有对应高架上）
        curLocation.parallelRoadFlag = flag; //0：无主辅路（车标所在道路旁无主辅路） 1：车标在主路（车标所在道路旁有辅路） 2：车标在辅路（车标所在道路旁有主路）
        mLastRouteOption.setCurrentLocation(curLocation);

        // 设置切换类型：高架切换 paralleTypeOverhead 还是 主辅路切换 paralleTypeMainSide，根据主辅路切换信息决定的
        if (switchRoadType == LocSwitchRoadType.LocSwitchUpBridgeToDownBridge || switchRoadType == LocSwitchRoadType.LocSwitchDownBridgeToUpBridge) {
            mLastRouteOption.setParalleType(paralleTypeOverhead);
        }
        if (switchRoadType == LocSwitchRoadType.LocSwitchMainToSide || switchRoadType == LocSwitchRoadType.LocSwitchSideToMain) {
            mLastRouteOption.setParalleType(paralleTypeMainSide);
        }

        // 发起路线重算
        final long requestId = mRouteService.requestRoute(mLastRouteOption);
        mRequestRouteId = requestId;
        final Hashtable<Long, RequestRouteResult> routeResultHashtable = mAdapterImplHelper.getRouteResultDataHashtable();
        routeResultHashtable.put(requestId, mLastRequestRouteResult);
        Logger.i(TAG, "平行路切换requestSwitchParallelRoute switchRoadType:"+switchRoadType+" roadID:"+roadID+" flag:"+flag+" hwFlag:"+hwFlag+" requestId:"+requestId);
        return requestId;
    }
}
