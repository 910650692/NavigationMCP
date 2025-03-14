package com.fy.navi.service.adapter.route.bls;

import static com.autonavi.gbl.util.errorcode.common.Service.ErrorCodeOK;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.aosclient.BLAosService;
import com.autonavi.gbl.common.model.ElecCommonParameter;
import com.autonavi.gbl.common.model.ElecCostList;
import com.autonavi.gbl.common.model.ElecInfoConfig;
import com.autonavi.gbl.common.model.ElecSpeedCostList;
import com.autonavi.gbl.common.model.PowertrainLoss;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.common.path.option.RouteOption;
import com.autonavi.gbl.route.RouteService;
import com.autonavi.gbl.route.model.RouteAlternativeChargeStationParam;
import com.autonavi.gbl.route.model.RouteChargingPreference;
import com.autonavi.gbl.route.model.RouteControlKey;
import com.autonavi.gbl.route.model.RouteInitParam;
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
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteAvoidInfo;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.utils.BevPowerCarUtils;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

/**
 * 算路服务.
 * @Description Impl类只做SDK的原子能力封装，不做对象及数据转换
 * @Author lvww
 * @date 2024/12/5
 */
public class RouteAdapterImpl implements IRouteApi {
    private static final String TAG = MapDefaultFinalTag.ROUTE_SERVICE_TAG;
    private RouteService mRouteService;
    private RouteAdapterImplHelper adapterImplHelper;

    public RouteAdapterImpl() {
        mRouteService = (RouteService) ServiceMgr.getServiceMgrInstance()
                .getBLService(SingleServiceID.RouteSingleServiceID);
        SceneModuleService mSceneModuleService = (SceneModuleService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.SceneModuleSingleServiceID);
        // 统一初始化场景组件服务
        InitSceneModuleParam param = new InitSceneModuleParam();
        param.bEnableDynamicCloudShowInfoModule = true; // 开启动态运营组件
        mSceneModuleService.init(param);
        adapterImplHelper = new RouteAdapterImplHelper(mRouteService, new BLAosService());
    }

    @Override
    public void initRouteService() {
        adapterImplHelper.initRouteService();
    }

    @Override
    public void registerRouteObserver(String key, RouteResultObserver routeResultObserver) {
        adapterImplHelper.registerRouteObserver(key, routeResultObserver);
    }

    @Override
    public void setRoutePreference(RoutePreferenceID routePreferenceID) {
        adapterImplHelper.setRoutePreference(routePreferenceID);
    }

    @Override
    public long requestRoute(MapTypeId mapTypeId, List<RouteParam> paramList, boolean fastNavi, int routeWay, boolean isOnline, int routeType) {
        adapterImplHelper.checkoutRouteServer();
        RequestRouteResult requestRouteResult = new RequestRouteResult();
        requestRouteResult.setMapTypeId(mapTypeId);
        requestRouteResult.setFastNavi(fastNavi);
        requestRouteResult.setRouteWay(routeWay);
        requestRouteResult.setOnlineRoute(isOnline);
        requestRouteResult.setRouteType(routeType);
        RouteOption routeOption = adapterImplHelper.getRequestParam(requestRouteResult, paramList);
        //设置共性参数 比如在线优先 ....
        //记录请求ID和屏幕ID以供返回结果使用
        long requestId = mRouteService.requestRoute(routeOption);
        Hashtable<Long, RequestRouteResult> routeResultHashtable =  adapterImplHelper.getRouteResultDataHashtable();
        routeResultHashtable.put(requestId, requestRouteResult);
        Logger.i(TAG, "route plane request id " + requestId);

        return requestId;
    }

    @Override
    public long requestRouteWeather(RouteLineLayerParam routeLineLayerParam, int index) {
        if (ConvertUtils.isEmpty(mRouteService)) return -1;
        return mRouteService.requestPathWeather((PathInfo) routeLineLayerParam.getPathInfoList().get(index));
    }

    @Override
    public long requestRouteAlternativeChargeStation(Object pathInfo,  String poiId) {
        if (ConvertUtils.isEmpty(mRouteService)) return -1;
        RouteAlternativeChargeStationParam param = new RouteAlternativeChargeStationParam();
        param.poiId = poiId;
        TaskResult taskResult = mRouteService.request((PathInfo) pathInfo, param, adapterImplHelper.getRouteAlternativeChargeStationObserver());
        Logger.d("requestRouteAlternativeChargeStation" + taskResult.errorCode);
        if (taskResult.errorCode != ErrorCodeOK) {
            return -1;
        }
        return taskResult.taskId;
    }

    @Override
    public boolean cancelRoute(long requestId) {
        return mRouteService.abortRequest(requestId);
    }

    @Override
    public void removeRouteObserver(String key) {
        adapterImplHelper.removeRouteObserver(key);
    }

    @Override
    public void unInitRouteService() {
        adapterImplHelper.removeAllObserver();
        if (ServiceInitStatus.ServiceInitDone != mRouteService.isInit()) {
            mRouteService.unInit();
        }
    }

    @Override
    public void setRoutePlan(boolean isNaviActive) {
        mRouteService.control(RouteControlKey.RouteControlKeySetInvoker, isNaviActive ? "navi" : "plan");
    }


    @Override
    public void setFamiliarRoute(boolean isFamiliarRoute) {
        mRouteService.control(RouteControlKey.RouteControlKeyPrivacy, isFamiliarRoute? "1" : "0");
    }

    @Override
    public void setAvoidRoad(RouteAvoidInfo routeAvoidInfo) {
        adapterImplHelper.setAvoidRoad(routeAvoidInfo);
    }

    //设置限行+车牌开关
    @Override
    public void setRestriction(String plateNumber, boolean isTrafficRestrictionOpen) {
        mRouteService.control(RouteControlKey.RouteControlKeyVehicleID, plateNumber );
        //设置eta请求的躲避车辆限行,0表示关闭eta限行请求，1 表示打开eta限行请求
        mRouteService.control(RouteControlKey.RouteControlKeyETARestriction, isTrafficRestrictionOpen ? "1" : "0");
        mRouteService.control(RouteControlKey.RouteControlKeySetTotalTime, "15000");
    }

    @Override
    public void setCarElecPlantion(boolean isCarElecPlanOpen) {
        mRouteService.control(RouteControlKey.RouteControlKeyVehicleType, BevPowerCarUtils.getInstance().carType);
        mRouteService.control(RouteControlKey.RouteControlKeepChargingStation, isCarElecPlanOpen ? "1" : "0");
        if (isCarElecPlanOpen) {
            RouteChargingPreference chargingPreference = new RouteChargingPreference();
            chargingPreference.brands.addAll(mRouteService.getSupportedChargingPreference().brands);
            mRouteService.setChargingPreference(chargingPreference);
            mRouteService.setElecInfoConfig(getElecInfoConfig());
            Logger.i(TAG, GsonUtils.toJson(getElecInfoConfig()));
        }
    }

    @Override
    public void setTrafficTrip(boolean isOpenTraffic) {
        mRouteService.control(RouteControlKey.RouteControlConfigSetTipsInfo, isOpenTraffic ? "1" : "0");
    }

    @Override
    public void sendEndEntity(PoiInfoEntity poiInfoEntity) {
        adapterImplHelper.sendEndEntity(poiInfoEntity);
    }

    @Override
    public void requestRouteRestoration(RouteMsgPushInfo routeMsgPushInfo, MapTypeId mapTypeId) {
        adapterImplHelper.checkoutRouteServer();
        // 路线还原
        Logger.i(TAG, "requestRouteRestoration");
        AimRoutePushMsg aimRoutePushMsg = (AimRoutePushMsg) routeMsgPushInfo.getMsgPushInfo();
        RouteRestorationOption routeRestorationOption = new RouteRestorationOption();
        RoutepathrestorationPathInfo path = aimRoutePushMsg.content.path;
        routeRestorationOption.setPaths(path.paths);
        routeRestorationOption.setStartPoints(path.startPoints.points);
        routeRestorationOption.setViaPoints(path.routeViaPoints.display_points, path.routeViaPoints.path_project_points);
        routeRestorationOption.setEndPoints(path.endPoints.points);

        MobileRouteParam routeParam = aimRoutePushMsg.content.routeParam;
        routeRestorationOption.setEndName(routeParam.destination.name);
        routeRestorationOption.setContentOption(routeParam.contentOption);

        routeRestorationOption.setRouteVer(RouteService.getRouteVersion());
        routeRestorationOption.setSdkVer(RouteService.getEngineVersion());

        MobileVehicleInfo vehicle = routeParam.vehicle;
        routeRestorationOption.setCarType(vehicle.type);
        routeRestorationOption.setCarSize(vehicle.size);
        routeRestorationOption.setCarHeight(vehicle.height);
        routeRestorationOption.setCarWidth(vehicle.width);
        routeRestorationOption.setCarLoad(vehicle.load);
        routeRestorationOption.setCarWeight(vehicle.weight);
        routeRestorationOption.setCarAxis(vehicle.axis);
        routeRestorationOption.setCarPlate(vehicle.plate);
        routeRestorationOption.setNaviId(aimRoutePushMsg.content.naviId);

        RequestRouteResult requestRouteResult = new RequestRouteResult();
        requestRouteResult.setMapTypeId(mapTypeId);
        requestRouteResult.setFastNavi(false);
        requestRouteResult.setOnlineRoute(true);
        requestRouteResult.setRouteWay(RouteWayID.ROUTE_WAY_DEFAULT);
        RouteLineLayerParam routeLineLayerParam = new RouteLineLayerParam();
        routeLineLayerParam.getRouteLinePoints().getEndPoints().add(routeMsgPushInfo.getEndPoint());
        routeLineLayerParam.getRouteLinePoints().getStartPoints().add(routeMsgPushInfo.getStartPoint());
        routeLineLayerParam.getRouteLinePoints().setViaPoints(routeMsgPushInfo.getViaPoints());
        requestRouteResult.setLineLayerParam(routeLineLayerParam);
        long requestId =  mRouteService.requestRouteRestoration(routeRestorationOption);
        Hashtable<Long, RequestRouteResult> routeResultHashtable =  adapterImplHelper.getRouteResultDataHashtable();
        routeResultHashtable.put(requestId, requestRouteResult);
        Logger.i(TAG, "route plane request id " + requestId);
    }

    private ElecInfoConfig getElecInfoConfig() {
        //设置电动车能耗参数
        ElecInfoConfig elecConfig = new ElecInfoConfig();
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
        ElecCostList costList = new ElecCostList();
        costList.auxValue = BevPowerCarUtils.getInstance().airConditioningOpen ? 1.0f : 0.5f; //空调...
        costList.ferryRate = BevPowerCarUtils.getInstance().ferryRate;
        ArrayList<ElecSpeedCostList> elecSpeedCostLists = new ArrayList<>();
        for (int t = 0; t < BevPowerCarUtils.getInstance().elecSpeedCostLists.size(); t++) {
            ElecSpeedCostList elecSpeedCostList = GsonUtils.convertToT(BevPowerCarUtils.getInstance().elecSpeedCostLists.get(t),ElecSpeedCostList.class);
            elecSpeedCostLists.add(elecSpeedCostList);
        }
        costList.speedCost = elecSpeedCostLists;
        ArrayList<PowertrainLoss> powertrainLossList = new ArrayList<>();
        for (int t = 0; t < BevPowerCarUtils.getInstance().powertrainLoss.size(); t++) {
            PowertrainLoss powertrainLoss = GsonUtils.convertToT(BevPowerCarUtils.getInstance().powertrainLoss.get(t),PowertrainLoss.class);
            powertrainLossList.add(powertrainLoss);
        }
        costList.powertrainLoss = powertrainLossList;
        ElecCommonParameter trans = new ElecCommonParameter(BevPowerCarUtils.getInstance().trans.access, BevPowerCarUtils.getInstance().trans.decess);
        costList.trans = trans;
        ElecCommonParameter curve = new ElecCommonParameter(BevPowerCarUtils.getInstance().curve.access, BevPowerCarUtils.getInstance().curve.decess);
        costList.curve = curve;
        ElecCommonParameter slope = new ElecCommonParameter(BevPowerCarUtils.getInstance().slope.access, BevPowerCarUtils.getInstance().slope.decess);
        costList.slope = slope;
        elecConfig.costList.add(costList);
        return elecConfig;
    }
}
