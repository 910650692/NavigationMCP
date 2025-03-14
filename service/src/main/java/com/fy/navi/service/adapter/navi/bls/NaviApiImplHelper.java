package com.fy.navi.service.adapter.navi.bls;

import com.android.utils.ConvertUtils;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.common.path.option.RouteType;
import com.autonavi.gbl.guide.GuideService;
import com.autonavi.gbl.guide.model.NaviPath;
import com.autonavi.gbl.guide.model.guidecontrol.CameraParam;
import com.autonavi.gbl.guide.model.guidecontrol.CommonParam;
import com.autonavi.gbl.guide.model.guidecontrol.Param;
import com.autonavi.gbl.guide.model.guidecontrol.Type;
import com.autonavi.gbl.guide.observer.INaviObserver;
import com.autonavi.gbl.guide.observer.ISoundPlayObserver;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.GuidanceObserver;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.cruise.CruiseParamEntity;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.navi.NaviParamEntity;

import java.util.ArrayList;
import java.util.Hashtable;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public class NaviApiImplHelper {
    private static final String TAG = MapDefaultFinalTag.NAVI_SERVICE_TAG;
    private final GuideService mGuideService;
    private RouteLineLayerParam mRouteLineLayerParam = new RouteLineLayerParam();
    private int mainIndex;
    private final INaviObserver naviObserver;
    private final ISoundPlayObserver soundPlayObserver;
    private final Hashtable<String, GuidanceObserver> mGuidanceObservers;
    private boolean mIsSimpleNavigation = false;

    protected NaviApiImplHelper(GuideService guideService) {
        this.mGuideService = guideService;
        mGuidanceObservers = new Hashtable<>();
        naviObserver = new GuidanceCallback(mGuidanceObservers);
        soundPlayObserver = new GuidanceCallback(mGuidanceObservers);
    }

    protected void initNaviService() {
        mGuideService.init();
        mGuideService.addNaviObserver(naviObserver);
        mGuideService.addSoundPlayObserver(soundPlayObserver);

    }

    protected void playTRManualExt(int requestId) {
        mGuideService.playTRManualExt(requestId);
    }

    protected void registerObserver(String key, GuidanceObserver guidanceObserver) {
        mGuidanceObservers.put(key, guidanceObserver);
    }

    public void unregisterObserver(String key) {
        mGuidanceObservers.remove(key);
    }

    protected void unit() {
        mGuideService.removeSoundPlayObserver(soundPlayObserver);
        mGuideService.removeNaviObserver(naviObserver);
        mGuidanceObservers.clear();
    }

    protected long obtainSAPAInfo(boolean isFindRemainPath) {
        return mGuideService.obtainSAPAInfo(isFindRemainPath);
    }

    protected void selectMainPathID(long pathID) {
        mGuideService.selectMainPathID(pathID);
    }

    protected void checkNaviService() {
        if (ConvertUtils.equals(ServiceInitStatus.ServiceNotInit, mGuideService.isInit())) {
            initNaviService();
        }
    }

    //设置开启导航路线
    protected void setNaviPathParam(int routeIndex, RouteLineLayerParam routeLineLayerParam) {
        mRouteLineLayerParam = routeLineLayerParam;
        mainIndex = routeIndex;
    }

    //获取开启导航关键参数
    protected NaviPath getNaviPathParam() {
        NaviPath mNaviPath = new NaviPath();
        mNaviPath.vecPaths = (ArrayList<PathInfo>) mRouteLineLayerParam.getPathInfoList(); // 设置完整路线信息 算路时会返回
        mNaviPath.mainIdx = mainIndex; // 设置主路线索引
        mNaviPath.type = RouteType.RouteTypeCommon; // 设置算路类型
        //mNaviPath.point = poiForRequest; // 用于GuideService偏航时组织行程点信息, 不影响路线绘制
        mNaviPath.strategy = 0x08 | 0x10; // 设置算路策略
        return mNaviPath;
    }

    /**
     * 初始化引导导航参数
     */
    public void initGuideParam() {
        Param param = new Param();
        param.type = Type.GuideParamNavi;//引导参数配置
        param.navi.v2x.enableCurveMeet = true; // 弯道会车预警(用户登录&&真实导航 生效)
        param.navi.v2x.enableCrossMeet = true; // 无灯路口会车预警(用户登录&&真实导航 生效)
        param.navi.naviScene = 0; //普通导航
//        param.navi.model = 1; // 多路线导航（备选路重算需要开启） 此处需要触发条件
        mGuideService.setParam(param);

        setCrossParam();
        setCameraParameters();
        setCommonParameters();
    }

    /***配置路口大图***/
    public void setCrossParam() {
        Param crossParam = new Param();
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
        Param camera = new Param();
        camera.type = Type.GuideParamCamera;//摄像头配置参数
        CameraParam cameraParam = camera.camera;
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
        CommonParam mCommonParam = new CommonParam();
        mCommonParam.enableAuto = true;
        Param param2 = new Param();
        param2.common = mCommonParam;
        mGuideService.setParam(param2);
    }


    //设置是否是轻导航
    protected void setSimpleNavigation(boolean isSimpleNavigaion) {
        mIsSimpleNavigation = isSimpleNavigaion;
    }

    /***配置导航播报开关***/
    public void updateGuideParam(NaviParamEntity naviParamEntity) {
        if (!ConvertUtils.isEmpty(naviParamEntity)) {
            Param param = new Param();
            param.type = naviParamEntity.getType();
            if (naviParamEntity.getType() == NaviConstant.GuideParamType.GuideParamTTSPlay) {
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

    public void setCruiseParam(CruiseParamEntity cruiseParamEntity) {
        if (!ConvertUtils.isEmpty(cruiseParamEntity)) {
            Param param = new Param();
            param.type = cruiseParamEntity.getType();
            if (cruiseParamEntity.getType() == NaviConstant.GuideParamType.GuideParamCruise) {
                param.cruise.cameraNum = cruiseParamEntity.getCameraNum();
                param.cruise.mode = cruiseParamEntity.getMode();
            } else if (cruiseParamEntity.getType() == NaviConstant.GuideParamType.GuideParamTR) {
                param.tr.enable = cruiseParamEntity.isTrEnable();
            }
            mGuideService.setParam(param);
        }
    }

    //获取是否是轻导航
    protected boolean getSimpleNavigation() {
        return mIsSimpleNavigation;
    }
}
