package com.sgm.navi.service.adapter.navi.bls;

import static com.autonavi.gbl.guide.model.guidecontrol.Type.GuideParamEmulator;

import android.annotation.SuppressLint;

import com.android.utils.ConvertUtils;
import com.android.utils.DeviceUtils;
import com.android.utils.TimeUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.ElecCommonParameter;
import com.autonavi.gbl.common.model.ElecCostList;
import com.autonavi.gbl.common.model.ElecInfoConfig;
import com.autonavi.gbl.common.model.ElecSpeedCostList;
import com.autonavi.gbl.common.model.PowertrainLoss;
import com.autonavi.gbl.common.model.TbtCommonControl;
import com.autonavi.gbl.common.model.UserConfig;
import com.autonavi.gbl.common.model.WorkPath;
import com.autonavi.gbl.common.path.model.ChargeStationInfo;
import com.autonavi.gbl.common.path.option.POIForRequest;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.guide.GuideService;
import com.autonavi.gbl.guide.model.NaviInfo;
import com.autonavi.gbl.guide.model.NaviPath;
import com.autonavi.gbl.guide.model.TimeAndDist;
import com.autonavi.gbl.guide.model.guidecontrol.CameraParam;
import com.autonavi.gbl.guide.model.guidecontrol.CommonParam;
import com.autonavi.gbl.guide.model.guidecontrol.EmulatorParam;
import com.autonavi.gbl.guide.model.guidecontrol.NaviParam;
import com.autonavi.gbl.guide.model.guidecontrol.Param;
import com.autonavi.gbl.guide.model.guidecontrol.Type;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.GuidanceObserver;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.cruise.CruiseParamEntity;
import com.sgm.navi.service.define.layer.RouteLineLayerParam;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.NaviParamEntity;
import com.sgm.navi.service.define.navi.NaviViaEntity;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.route.ChargingInfo;
import com.sgm.navi.service.define.utils.BevPowerCarUtils;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

/**
 * impl辅助类
 *
 * @author sgm
 * @version $Revision.*$
 */
public class NaviApiImplHelper {
    private static final String TAG = MapDefaultFinalTag.NAVI_SERVICE_TAG;
    private GuideService mGuideService;
    private final GuidanceCallback mNaviObserver;
    private final Hashtable<String, GuidanceObserver> mGuidanceObservers;
    private boolean mIsSimpleNavigation = false;
    // 当前导航信息
    protected NaviInfo mNaviInfo;
    private RouteLineLayerParam mRouteLineLayerParam;
    private int mainIndex;

    protected NaviApiImplHelper(final GuideService guideService) {
        this.mGuideService = guideService;
        mGuidanceObservers = new Hashtable<>();
        mNaviObserver = new GuidanceCallback(mGuidanceObservers, this);
    }

    protected void initNaviService() {
        initTbtComm();
        if (mGuideService == null) {
            Logger.e(TAG, "GuideService is null, please check the service registration.");
            // 如果GuideService未初始化，则从ServiceMgr获取，再获取一次
            mGuideService = (GuideService) ServiceMgr.getServiceMgrInstance()
                    .getBLService(SingleServiceID.GuideSingleServiceID);
        }
        mGuideService.init();
        mGuideService.addNaviObserver(mNaviObserver);
        mGuideService.addSoundPlayObserver(mNaviObserver);
    }

    /**
     * 初始化公共控制类
     */
    private void initTbtComm() {
        final String cache = GBLCacheFilePath.TBT_COMMON_CACHE_PATH;
        final String navi = GBLCacheFilePath.OFFLINE_DOWNLOAD_DIR;
        final WorkPath workPath = new WorkPath();
        workPath.cache = cache;
        workPath.navi = navi;
        final UserConfig userConfig = new UserConfig();
        userConfig.deviceID = DeviceUtils.getDeviceId();
        userConfig.userBatch = "0";
        final TbtCommonControl tbtCommonControl = TbtCommonControl.getInstance();
        tbtCommonControl.init(workPath, userConfig);
    }

    protected void playTRManualExt(final int requestId) {
        mGuideService.playTRManualExt(requestId);
    }

    protected void registerObserver(final String key, final GuidanceObserver guidanceObserver) {
        mGuidanceObservers.put(key, guidanceObserver);
    }

    /**
     * @param key key
     */
    public void unregisterObserver(final String key) {
        mGuidanceObservers.remove(key);
    }

    protected void unit() {
        mGuideService.removeSoundPlayObserver(mNaviObserver);
        mGuideService.removeNaviObserver(mNaviObserver);
        mGuidanceObservers.clear();
    }

    /**
     * @param isFindRemainPath true：查询剩余路线上的sapa数据 f
     *                         alse：查询当前车辆所在高速路段上的sapa（车辆必须在高速上）
     * @return 返回和请求的唯一ID，与回调中的requestId保持一致，获取异常返回0
     * TODO：此方法目前没有被调用过
     */
    protected long obtainSAPAInfo(final boolean isFindRemainPath) {
        return mGuideService.obtainSAPAInfo(isFindRemainPath);
    }

    protected void selectMainPathID(final long pathID) {
        mGuideService.selectMainPathID(pathID);
    }

    protected void checkNaviService() {
        if (ConvertUtils.equals(ServiceInitStatus.ServiceNotInit, mGuideService.isInit())) {
            initNaviService();
        }
    }

    //设置开启导航路线
    protected void updateNaviPathParam(final RouteLineLayerParam routeLineLayerParam) {
        checkNaviService();
        //导航中更新路线发送加热数据
        mNaviObserver.onBatterHotTime();
        final NaviPath naviPath = new NaviPath();
        // 设置完整路线信息 算路时会返回
        naviPath.vecPaths = (ArrayList<PathInfo>) routeLineLayerParam.getMPathInfoList();
        naviPath.mainIdx = routeLineLayerParam.getMSelectIndex(); // 设置主路线索引
        naviPath.type = routeLineLayerParam.getMRouteType(); // 设置算路类型
        naviPath.point = (POIForRequest) routeLineLayerParam.getMPoiForRequest(); // 用于GuideService偏航时组织行程点信息, 不影响路线绘制
        naviPath.strategy = routeLineLayerParam.getMStrategy(); // 设置算路策略
        mGuideService.setNaviPath(naviPath);
    }

    //设置开启导航路线
    protected void setNaviPathParam(final int routeIndex, final RouteLineLayerParam routeLineLayerParam) {
        mRouteLineLayerParam = routeLineLayerParam;
        mainIndex = routeIndex;
    }

    //获取开启导航关键参数
    protected NaviPath getNaviPathParam() {
        if (ConvertUtils.isEmpty(mRouteLineLayerParam)) {
            Logger.e(TAG, "getNaviPathParam: mRouteLineLayerParam is null");
            return null;
        }
        //开始导航时发送加热数据
        mNaviObserver.onBatterHotTime();
        final NaviPath naviPath = new NaviPath();
        // 设置完整路线信息 算路时会返回
        naviPath.vecPaths = (ArrayList<PathInfo>) mRouteLineLayerParam.getMPathInfoList();
        naviPath.mainIdx = mainIndex; // 设置主路线索引
        naviPath.type = mRouteLineLayerParam.getMRouteType(); // 设置算路类型
        naviPath.point = (POIForRequest) mRouteLineLayerParam.getMPoiForRequest(); // 用于GuideService偏航时组织行程点信息, 不影响路线绘制
        naviPath.strategy = mRouteLineLayerParam.getMStrategy(); // 设置算路策略
        return naviPath;
    }

    /**
     * 初始化引导导航参数
     */
    public void initGuideParam() {
        final Param param = new Param();
        param.type = Type.GuideParamNavi;//引导参数配置
        param.navi.v2x.enableCurveMeet = true; // 弯道会车预警(用户登录&&真实导航 生效)
        param.navi.v2x.enableCrossMeet = true; // 无灯路口会车预警(用户登录&&真实导航 生效)
        param.navi.naviScene = 0; //普通导航
        EmulatorParam emulator = new EmulatorParam();
        emulator.speed = 180;//模拟导航车速
        param.emulator = emulator;
        NaviParam naviParam = new NaviParam();
        naviParam.model = 1;
        param.navi = naviParam; // 多路线导航
        mGuideService.setParam(param);

        setCrossParam();
        setCameraParameters();
        setCommonParameters();
        setElectInfoConfig();
    }

    /***配置路口大图***/
    public void setCrossParam() {
        final Param crossParam = new Param();
        crossParam.type = Type.GuideParamCrossing;//放大图配置参数配置
        crossParam.crossing.enable3D = true; //  三维总开关
        crossParam.crossing.enableVectorImage = true; // 矢量图显示开
        crossParam.crossing.enableGridImage = true; // 栅格图显示开关
        crossParam.crossing.isMultiCross = true; // 是否一个路口支持多类型大图透出
//        crossParam.crossing.isDayForUseSet = !NightModeGlobal.isNightMode();    // 昼夜模式
        mGuideService.setParam(crossParam);
    }

    /***配置摄像头参数配置***/
    public void setCameraParameters() {
        final Param camera = new Param();
        camera.type = Type.GuideParamCamera;//摄像头配置参数
        final CameraParam cameraParam = camera.camera;
        cameraParam.enable = true;       /* 打开摄像头显示 */
        cameraParam.maxCount = 5;        /* 摄像头显示个数为5个 */
        cameraParam.checkDistance = new int[]{1000, 1000, 500};
        cameraParam.checkDistance[0] = 5000;    /* 高速公路 */
        cameraParam.checkDistance[1] = 5000;    /* 主要大街、城市快速道 */
        cameraParam.checkDistance[2] = 5000;    /* 其他道路 */
        mGuideService.setParam(camera);
    }

    /***配置公共参数配置***/
    public void setCommonParameters() {
        // 配置导航播报开关
        final CommonParam commonParam = new CommonParam();
        commonParam.enableAuto = true;
        final Param param2 = new Param();
        param2.common = commonParam;
        mGuideService.setParam(param2);
    }

    public void setElectInfoConfig() {
        // 判断是否是电车需要设置能耗参数
        if (CalibrationPackage.getInstance().powerType() == 1) {
            mGuideService.setElecInfoConfig(getElecInfoConfig());
        }
    }

    public void startNavi(boolean startNaviStatus) {
        if (startNaviStatus) mNaviObserver.startNavi();
    }


    //设置是否是轻导航
    protected void setSimpleNavigation(final boolean isSimpleNavigaion) {
        mIsSimpleNavigation = isSimpleNavigaion;
    }

    /**
     * 配置导航播报开关
     *
     * @param naviParamEntity entity
     **/
    @SuppressLint("WrongConstant")
    public void updateGuideParam(final NaviParamEntity naviParamEntity) {
        if (!ConvertUtils.isEmpty(naviParamEntity)) {
            final Param param = new Param();
            param.type = naviParamEntity.getType();
            if (naviParamEntity.getType() == NaviConstant.GuideParamType.GUIDE_PARAM_TTS_PLAY) {
                param.tts.style = naviParamEntity.getStyle();
                //打开区域播报
                param.tts.enableADCode = naviParamEntity.isEnableADCode();
                //关闭疲劳驾驶  设置疲劳驾驶播报选项0:TBT自个控制播报 1 : TBT播报，但播报条件由第三方设置给TBT 2 : TBT完全不播报。默认值为0
                param.tts.fatiguedTTS = naviParamEntity.getFatiguedTTS();
                param.tts.isDay = naviParamEntity.isDay();
            }
            mGuideService.setParam(param);
        }
    }

    /**
     * @param cruiseParamEntity entity
     */
    public void setCruiseParam(final CruiseParamEntity cruiseParamEntity) {
        if (!ConvertUtils.isEmpty(cruiseParamEntity)) {
            final Param param = new Param();
            param.type = cruiseParamEntity.getType();
            if (cruiseParamEntity.getType() == NaviConstant.GuideParamType.GUIDE_PARAM_CRUISE) {
                param.cruise.cameraNum = cruiseParamEntity.getCameraNum();
                param.cruise.mode = cruiseParamEntity.getMode();
            } else if (cruiseParamEntity.getType() == NaviConstant.GuideParamType.GUIDE_PARAM_TR) {
                param.tr.enable = cruiseParamEntity.isTrEnable();
            }
            mGuideService.setParam(param);
        }
    }

    //获取是否是轻导航
    protected boolean getSimpleNavigation() {
        return mIsSimpleNavigation;
    }

    // TODO 参照RouteAdapterImpl, 后续还要修改
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
        ElecCostList costList = new ElecCostList();
        costList.auxValue = BevPowerCarUtils.getInstance().airConditioningOpen ? 1.0f : 0.5f; //空调...
        costList.ferryRate = BevPowerCarUtils.getInstance().ferryRate;
        final ArrayList<ElecSpeedCostList> elecSpeedCostLists = new ArrayList<>();
        for (int t = 0; t < BevPowerCarUtils.getInstance().elecSpeedCostLists.size(); t++) {
            ElecSpeedCostList elecSpeedCostList = GsonUtils.convertToT(BevPowerCarUtils.getInstance().elecSpeedCostLists.get(t), ElecSpeedCostList.class);
            elecSpeedCostLists.add(elecSpeedCostList);
        }
        costList.speedCost = elecSpeedCostLists;
        final ArrayList<PowertrainLoss> powertrainLossList = new ArrayList<>();
        for (int t = 0; t < BevPowerCarUtils.getInstance().powertrainLoss.size(); t++) {
            PowertrainLoss powertrainLoss = GsonUtils.convertToT(BevPowerCarUtils.getInstance().powertrainLoss.get(t), PowertrainLoss.class);
            powertrainLossList.add(powertrainLoss);
        }
        costList.powertrainLoss = powertrainLossList;
        final ElecCommonParameter trans = new ElecCommonParameter(BevPowerCarUtils.getInstance().trans.access, BevPowerCarUtils.getInstance().trans.decess);
        costList.trans = trans;
        final ElecCommonParameter curve = new ElecCommonParameter(BevPowerCarUtils.getInstance().curve.access, BevPowerCarUtils.getInstance().curve.decess);
        costList.curve = curve;
        final ElecCommonParameter slope = new ElecCommonParameter(BevPowerCarUtils.getInstance().slope.access, BevPowerCarUtils.getInstance().slope.decess);
        costList.slope = slope;
        elecConfig.costList.add(costList);
        return elecConfig;
    }

    /***
     * 此接口属于动态获取
     * @return 获取开启补能规划后自动添加的途径点
     */
    public List<NaviViaEntity> getAllViaPoints(PathInfo pathInfo) {
        if (pathInfo == null || mNaviInfo == null) {
            return new ArrayList<>();
        }
        final ArrayList<NaviViaEntity> naviViaEntities = new ArrayList<>();
        final ArrayList<ChargeStationInfo> chargeStationInfos = pathInfo.getChargeStationInfo();
        final ArrayList<TimeAndDist> remainList = mNaviInfo.ChargeStationRemain;
        if (ConvertUtils.isEmpty(chargeStationInfos) || ConvertUtils.isEmpty(remainList)) {
            return new ArrayList<>();
        }
        Logger.i(TAG, "getAllViaPoints:", chargeStationInfos.size(), "remainSize:",
                remainList.size());
        final int size = Math.min(chargeStationInfos.size(), remainList.size());
        for (int i = 0; i < size; i++) {
            final ChargeStationInfo stationInfo = chargeStationInfos.get(i);
            final TimeAndDist timeAndDist = remainList.get(i);

            final NaviViaEntity naviViaEntity = new NaviViaEntity();
            naviViaEntity.setName(stationInfo.name);
            naviViaEntity.setArriveDay(TimeUtils.getArriveDay(timeAndDist.time));
            naviViaEntity.setDistance(TimeUtils.getRemainInfo(AppCache.getInstance().getMContext(), timeAndDist.dist, timeAndDist.time));
            naviViaEntity.setArriveTime(TimeUtils.getArriveTime(AppCache.getInstance().getMContext(), timeAndDist.time));
            naviViaEntity.setmArriveTimeStamp(timeAndDist.time);
            naviViaEntities.add(naviViaEntity);

            final ChargingInfo chargingInfo = new ChargingInfo();
            chargingInfo.setAutoAdd(true);
            chargingInfo.setMMinArrivalPercent((short) stationInfo.remainingPercent);
            chargingInfo.setChargeTime(stationInfo.chargeTime);
            naviViaEntity.setChargeInfo(chargingInfo);

            naviViaEntity.setRealPos(new GeoPoint(stationInfo.show.lon, stationInfo.show.lat));
            naviViaEntity.setPid(stationInfo.poiID);
        }
        return naviViaEntities;
    }

    public boolean pauseNavi(long naviId) {
        Logger.d(TAG, "pauseNavi");
        return mGuideService != null && mGuideService.pauseNavi(naviId);
    }

    public boolean resumeNavi(long naviId) {
        Logger.d(TAG, "resumeNavi");
        return mGuideService != null && mGuideService.resumeNavi(naviId);
    }

    public void setSimulationSpeedParam(int simulationSpeed) {
        //模拟导航配置参数
        EmulatorParam mEmulatorParam = new EmulatorParam();
        mEmulatorParam.speed = simulationSpeed;
        Param param = new Param();
        param.type = GuideParamEmulator;
        param.emulator = mEmulatorParam;
        mGuideService.setParam(param);
    }
}
