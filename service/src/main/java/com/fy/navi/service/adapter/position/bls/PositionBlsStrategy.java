package com.fy.navi.service.adapter.position.bls;

import android.content.Context;
import android.location.LocationManager;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.os.SystemClock;
import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.ToastUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.LooperType;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.pos.PosService;
import com.autonavi.gbl.pos.model.DrInfo;
import com.autonavi.gbl.pos.model.GPSDatetime;
import com.autonavi.gbl.pos.model.GraspRoadResult;
import com.autonavi.gbl.pos.model.LocDataType;
import com.autonavi.gbl.pos.model.LocFeedbackNode;
import com.autonavi.gbl.pos.model.LocGnss;
import com.autonavi.gbl.pos.model.LocInfo;
import com.autonavi.gbl.pos.model.LocMMFeedbackInfo;
import com.autonavi.gbl.pos.model.LocMatchInfo;
import com.autonavi.gbl.pos.model.LocModeType;
import com.autonavi.gbl.pos.model.LocSignData;
import com.autonavi.gbl.pos.model.PosWorkPath;
import com.autonavi.gbl.pos.observer.IPosDrInfoObserver;
import com.autonavi.gbl.pos.observer.IPosGraspRoadResultObserver;
import com.autonavi.gbl.pos.observer.IPosLocInfoObserver;
import com.autonavi.gbl.pos.observer.IPosMapMatchFeedbackObserver;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.user.usertrack.model.GpsTrackPoint;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.position.IPositionAdapterCallback;
import com.fy.navi.service.adapter.position.PositionConstant;
import com.fy.navi.service.adapter.position.bls.comm.GpsStatusChecker;
import com.fy.navi.service.adapter.position.bls.comm.LocationFuncSwitch;
import com.fy.navi.service.adapter.position.bls.sensor.MountAngleManager;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.navi.L2NaviBean;
import com.fy.navi.service.define.position.DrBean;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.position.LocMMInfo;
import com.fy.navi.service.define.position.LocMode;
import com.fy.navi.service.define.position.LocStatus;
import com.fy.navi.service.define.position.PositionConfig;
import com.fy.navi.service.define.setting.SettingConstant;
import com.fy.navi.service.define.user.usertrack.GpsTrackPointBean;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.math.BigInteger;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

/***定位模块细分管理***/
public class PositionBlsStrategy implements IPosLocInfoObserver, IPosMapMatchFeedbackObserver, IPosDrInfoObserver,
        GpsStatusChecker.OnTimeOutCallback, Handler.Callback, IPosGraspRoadResultObserver {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private static final long SAVE_LOC_INTERVAL = 300 * 1000;// 五分钟存一次
    public static final int GPS_PROVIDER_SOURCE_TYPE = 0;
    public static final int NETWORK_PROVIDER_SOURCE_TYPE = 1;
    private static final String MMF_KEY = "AmapAutoMMF";
    private static final List<IPositionAdapterCallback> callbacks = new CopyOnWriteArrayList<>();
    private static final DrBean drInfo = new DrBean();
    private GpsStatusChecker mGpsStatusChecker;
    /* 定位状态*/
    protected LocStatus mSdkLocStatus = LocStatus.ON_LOCATION_GPS_FAIl;
    //位置信息
    private LocInfoBean locInfoBean;
    private LocInfoBean mCurDrLocation;
    private PosService mPosService;
    private LocMode mLocMode;
    private LocationManager mLocationManager;
    private PositionConfig mPositionConfig;
    private ScheduledFuture mSaveLocationFuture;
    private Runnable mCustomTimer;
    private Handler mH;
    private static final int MSG_LOC_DR_INFO_UPDATE = 1;
    private static final int MSG_LOC_STATUS_UPDATE = 2;
    private static final int MSG_LOC_GNSS_INFO_UPDATE = 3;
    private static final int MSG_SEND_DR = 4;
    private long mLastLocTimeStamp = -1;
    private boolean mIsLocSuccess;//是否定位成功
    private int mGear = -1;
    private long mLastSaveTimes;
    private PosParallelRoadController mPosParallelRoadController;
    public boolean mAutoNaviPosLogEnable = false;
    private GpsTrackPointBean mGpsTrackPoint = new GpsTrackPointBean();


    public PositionBlsStrategy(Context context) {
        mPosService = (PosService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.PosSingleServiceID);
        this.mLocationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
        MountAngleManager.MountAngleInfo mountAngleInfo = MountAngleManager.getInstance().getMountAngleInfo();
        this.mPositionConfig = new PositionConfig().setRoll(mountAngleInfo.roll)
                .setYaw(mountAngleInfo.yaw)
                .setPitch(mountAngleInfo.pitch);
        mH = new Handler(ThreadManager.getInstance().getLooper(LooperType.LocInfoUpdate), this);
        mPosParallelRoadController = new PosParallelRoadController(mPosService, callbacks);
    }

    /**
     * 初始化定位模块
     *
     * @param locMode        定位工作模式结构体
     * @param positionConfig
     */
    public boolean initLocEngine(LocMode locMode, PositionConfig positionConfig) {
        mLocMode = locMode;
        PosWorkPath posWorkPath = new PosWorkPath();
        String posRootDir = GBLCacheFilePath.POS_DIR + "pos";
        String contextDir = posRootDir + "/context/";
        FileUtils.getInstance().createDir(posRootDir, false);
        FileUtils.getInstance().createDir(contextDir, false);
        //posWorkPath.locPath = posLogDir;//该方法已废弃 统一通过BaseInitParam.logPath配置
        posWorkPath.contextPath = contextDir;//存放定位上下文文件目录
        LocModeType locModeType = LocationFuncSwitch.getLocModeType(locMode, mPositionConfig);
        initLocation();
        mPosService.setDefaultPos(new Coord3DDouble(locInfoBean.getLongitude(), locInfoBean.getLatitude(), locInfoBean.getAltitude()));
        // 添加位置信息观察者
        mPosService.addLocInfoObserver(this, 0);
        //添加地图反馈观察者
        mPosService.addMapMatchFeedbackObserver(this);
        //l2++使用，用以获取道路等级及Link信息
        mPosService.addGraspRoadResultObserver(this);
        mPosParallelRoadController.addObserver();
        // 定位服务初始化
        int resultCode = mPosService.init(posWorkPath, locModeType);
        Logger.d(TAG, "initLocEngine :" + resultCode + "\n" +
                "contextDir：" + contextDir);
        if (resultCode == 0) {
            initGpsStatusChecker();
            startLocStorageTimerTask();
            // 信号记录功能开关
            mPosService.signalRecordSwitch(mAutoNaviPosLogEnable, LocationFuncSwitch.getLocLogConf());
//            mPosService.signalRecordSwitch(true, LocationFuncSwitch.getLocLogConf());
            return true;
        }
        return false;
    }


    /**
     * 反初始化定位模块
     */
    public void uninitLocEngine() {
        saveLocStorage();
        uninitGpsStatusChecker();
        stopLocStorageTimerTask();
        mH.removeCallbacksAndMessages(null);
        mPosParallelRoadController.removeObserver();
        // 添加位置信息观察者
        mPosService.removeLocInfoObserver(this);
        //添加地图反馈观察者
        mPosService.removeMapMatchFeedbackObserver(this);
    }

    public PosService getPosService() {
        return mPosService;
    }


    public void registerCallback(IPositionAdapterCallback callback) {
        if (!callbacks.contains(callback)) {
            callbacks.add(callback);
        }
    }

    public void unregisterCallback() {
        for (IPositionAdapterCallback callback : callbacks) {
            callbacks.remove(callback);
        }
    }

    // PosService.setSignInfo() 会触发此回调

    /**
     * 引擎对外通知位置更新，输出频率10HZ
     *
     * @attention 此接口是在引擎线程内触发的，严禁做大规模运算或调用可能导致线程挂起的接口，如IO操作、同步类接口(同步DBUS)等。
     * @attention onParallelRoadUpdate接口由定位线程调用，如果有访问临界区需要做保护。但不建议使用过多的锁。
     */
    @Override
    public void onLocInfoUpdate(LocInfo locInfo) {
        if (null == locInfo) {
            Logger.e(TAG, "onLocInfoUpdate: locInfo=null and return");
            return;
        }
        //模拟导航无需保存位置信息
        if (locInfo.isSimulate == 1) {
            return;
        }
        updateLocation(locInfoBean, locInfo);
        if (locInfoBean != null) {
            mSdkLocStatus = LocStatus.ON_LOCATION_OK;
        }
        if (!ConvertUtils.isEmpty(callbacks)) {
            if (SystemClock.elapsedRealtime() - mLastLocTimeStamp < 1000) {
                return;
            }
            mLastLocTimeStamp = SystemClock.elapsedRealtime();
            if (mH.hasMessages(MSG_LOC_DR_INFO_UPDATE)) {
                mH.removeMessages(MSG_LOC_DR_INFO_UPDATE);
            }
            mH.sendEmptyMessage(MSG_LOC_DR_INFO_UPDATE);
        }

        //gps打点
        ArrayList<LocMatchInfo> matchInfo = locInfo.matchInfo;
        int matchInfoSize = matchInfo == null ? 0 : matchInfo.size();

        if (matchInfoSize > 0) {
            LocMatchInfo locMatchInfo = matchInfo.get(0);
            mGpsTrackPoint.setF32Course(locMatchInfo.course);
            mGpsTrackPoint.setF64Latitude(locMatchInfo.stPos.lat);
            mGpsTrackPoint.setF64Longitude(locMatchInfo.stPos.lon);
        }

        float curSpeed = locInfo.speed;
        mGpsTrackPoint.setF32Accuracy(locInfo.posAcc);
        mGpsTrackPoint.setF32Speed(curSpeed);
        mGpsTrackPoint.setF64Altitude(locInfo.alt);
        mGpsTrackPoint.setN64TickTime(parseGpsDateTime(locInfo.gpsDatetime));
        for (IPositionAdapterCallback callback : callbacks) {
            callback.onGpsTrackPoint(mGpsTrackPoint);
        }
    }

    public static long parseGpsDateTime(GPSDatetime gpsDatetime) {
        if (gpsDatetime.year == 0) {
            return System.currentTimeMillis();
        }
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        try {
            Date date = simpleDateFormat.parse(
                    gpsDatetime.year + "-" + gpsDatetime.month + "-" + gpsDatetime.day + " " + gpsDatetime.hour + ":" + gpsDatetime.minute + ":" +
                            gpsDatetime.second);
            return date.getTime();
        } catch (ParseException e) {

            return System.currentTimeMillis();
        }
    }

    /**
     * Dr融合位置（匹配前和匹配后）及相关信息透出给外部使用
     * 此接口是在引擎线程内触发的，严禁做大规模运算或调用可能导致线程挂起的接口，如IO操作、同步类接口(同步DBUS)等。
     */
    @Override
    public void onDrInfoUpdate(DrInfo drInfo) {
        this.drInfo.drRawPos.setLat(drInfo.drRawPos.lat);
        this.drInfo.drRawPos.setLon(drInfo.drRawPos.lon);
        this.drInfo.drRawPos.setZ(drInfo.drRawPos.z);
        this.drInfo.aziAcc = drInfo.aziAcc;
        this.drInfo.drMatchAzi = drInfo.drMatchAzi;
        this.drInfo.tickTime = drInfo.tickTime;
        this.drInfo.gpsStatus = drInfo.gpsStatus;
        this.drInfo.drAzi = drInfo.drAzi;
        this.drInfo.spd = drInfo.spd;
        this.drInfo.posAcc = drInfo.posAcc;
        this.drInfo.moveStatus = drInfo.moveStatus;
        this.drInfo.drStatus = drInfo.drStatus;
        this.drInfo.sceneState = drInfo.sceneState;
        this.drInfo.deltaBearing = drInfo.deltaBearing;
        this.drInfo.deltaPos = drInfo.deltaPos;
        this.drInfo.pluseSpd = drInfo.pluseSpd;
        this.drInfo.deltaAlt = drInfo.deltaAlt;
        this.drInfo.deltaAltAcc = drInfo.deltaAltAcc;
        this.drInfo.slopeValue = drInfo.slopeValue;
        this.drInfo.slopeAcc = drInfo.slopeAcc;
        this.drInfo.moveDist = drInfo.moveDist;
        this.drInfo.bMountAngleReady = drInfo.bMountAngleReady;
        this.drInfo.matchStatus = drInfo.matchStatus;
        this.drInfo.drMatchPos.setZ(drInfo.drMatchPos.z);
        this.drInfo.drMatchPos.setLon(drInfo.drMatchPos.lon);
        this.drInfo.drMatchPos.setLat(drInfo.drMatchPos.lat);
        if (mH.hasMessages(MSG_SEND_DR)) {
            mH.removeMessages(MSG_SEND_DR);
        }
        mH.sendEmptyMessage(MSG_SEND_DR);
    }

    public LocInfoBean getLastCarLocation() {
        return locInfoBean;
    }

    /**
     * 地图匹配反馈观察者,仅前端融合有效,如果缺失会影响定位准确性。
     * 航位推算(DR)由于受传感器精度、季
     * 漂、温漂、刻度系数误差、安装误差等影响,会持续累积误差,精度下
     * 降;MMF可以提供较为准确的位置信息用于辅助修正航位推建算误差,
     * 主要提升隧道、高架桥下、城市峡谷等GPS中断、漂移区域的的定位效
     * 果;
     * 不对接MMF理论上对隧道、城市峡谷、高架桥下的DR效果影响最大,
     * 会出现车标漂移、错误偏航等一系列问题,尤其是长隧道几呼必然会出
     * 现DR漂移的问题,漂移距离可能会达到公里级(视隧道长度)。
     *
     * @param locMMFeedbackInfo
     */
    @Override
    public void onMapMatchFeedbackUpdate(LocMMFeedbackInfo locMMFeedbackInfo) {
        if (locMMFeedbackInfo == null) {
            return;
        }
        String mmfInfo = getMMFJson(locMMFeedbackInfo);
        if (TextUtils.isEmpty(mmfInfo)) {
            return;
        }
        Bundle bundle = new Bundle();
        bundle.putString("AmapAutoMMF", mmfInfo);
        mLocationManager.sendExtraCommand(selectProvider(), MMF_KEY, bundle);
        LocMMInfo locMMInfo = GsonUtils.convertToT(locMMFeedbackInfo, LocMMInfo.class);
        // Logger.d("xqniu-----locMMFeedbackInfo" + GsonUtils.toJson(locMMFeedbackInfo) + "\n locMMInfo:" + GsonUtils.toJson(locMMInfo));
        for (IPositionAdapterCallback callback : callbacks) {
            callback.onMapMatchFeedbackUpdate(locMMInfo);
        }
    }

    /**
     * 将sdk内部返回的地图匹配反馈信息转换成JSON格式。
     * 将地图匹配的结果通知给 DR 模块（地图匹配结果依托于道路数据）
     * MMF信息只在巡航匹配在道路上时才有效，如果巡航脱离道路(如地下停车场等场景)则返回的MMF信息是空的。即道路数量为0，匹配点经纬度、道路属性等所有信息均无效。
     *
     * @param locMMFeedback
     * @return
     */
    private String getMMFJson(LocMMFeedbackInfo locMMFeedback) {
        String mmfInfo = null;
        try {
            JSONObject jsonObject = new JSONObject();
            jsonObject.put("ticktime", String.valueOf(locMMFeedback.ticktime));
            JSONArray jsonArray = new JSONArray();
            ArrayList<LocFeedbackNode> locFeedbackNodes = locMMFeedback.feedbackNodes;
            if (locFeedbackNodes == null || locFeedbackNodes.size() <= 0) {
                return null;
            }
            for (int i = 0; i < locFeedbackNodes.size(); i++) {
                JSONObject object = new JSONObject();
                LocFeedbackNode feedbackNode = locFeedbackNodes.get(i);
                Coord3DDouble deltaPoint = feedbackNode.deltaPoint;
                if (deltaPoint != null) {
                    object.put("lon", deltaPoint.lon);
                    object.put("lat", deltaPoint.lat);
                    object.put("zLevel", deltaPoint.z);
                }
                object.put("roadAzi", feedbackNode.roadAzi);
                object.put("probability", feedbackNode.probability);
                object.put("feedBackType", feedbackNode.type);
                object.put("roadWidth", feedbackNode.roadWidth);
                jsonArray.put(i, object);
            }
            jsonObject.put("count", locMMFeedback.count);
            jsonObject.put("feedbackNodes", jsonArray);
            mmfInfo = jsonObject.toString();
        } catch (JSONException e) {

        }
        return mmfInfo;
    }

    private String selectProvider() {
        if (mLocMode == LocMode.GNSS) {
            return LocationManager.NETWORK_PROVIDER;
        }
        return LocationManager.GPS_PROVIDER;
    }

    public void doStopLocate() {

    }

    /***切换主辅路、高架***/
    public void switchParallelRoad(int switchRoadType, BigInteger roadId) {
        if (mPosService != null) {
            mPosService.switchParallelRoad(switchRoadType, roadId);
        }
    }

    /**
     * 更新标定文件中的位置信息
     */
    public void saveLocStorage() {
        if (mPosService != null) {
            mPosService.saveLocStorage();
        }
    }

    public GeoPoint wgs84ToGcj02(GeoPoint coordinate) {
        Coord3DDouble coord3DDouble = new Coord3DDouble(coordinate.getLon(), coordinate.getLat(), coordinate.getZ());
        Coord3DDouble gcjCoordinate = PosService.encryptLonLat(coord3DDouble);
        GeoPoint gcj02Point = new GeoPoint(gcjCoordinate.lon, gcjCoordinate.lat, gcjCoordinate.z);
        return gcj02Point;
    }

    /**
     * 设置GNSS定位信息
     *
     * @param gnssInfo LocDataType.LocDataGnss
     */
    public void setGnssInfo(LocGnss gnssInfo) {
        if (!checkValid() || mGpsStatusChecker == null) {
            return;
        }
        mGpsStatusChecker.clearCount();
        mSdkLocStatus = LocStatus.ON_LOCATION_GPS_OK;
        updateSdkLocStatus(true);
        LocSignData data = new LocSignData();
        data.dataType = LocDataType.LocDataGnss;
        data.gnss = gnssInfo;
        if (mPosService != null) {
            mPosService.setSignInfo(data);
        }
    }

    /**
     * 设置车速脉冲信号、加速度计、陀螺仪、设置卫星星历数据
     */
    public void setSignInfo(LocSignData locSignData) {
        if (mPosService != null) {
            mPosService.setSignInfo(locSignData);
        }
    }

    private boolean checkValid() {
        return mPosService != null && mPosService.isInit() == ServiceInitStatus.ServiceInitDone;
    }

    @Override
    public void onTimeOut() {
        mSdkLocStatus = LocStatus.ON_LOCATION_FAIL;
        updateSdkLocStatus(false);
    }

    private void startLocStorageTimerTask() {
        Logger.i(TAG, "startLocStorageTimerTask");
        stopLocStorageTimerTask();
        mCustomTimer = new Runnable() {
            @Override
            public void run() {
                saveLocStorage();
            }
        };
        mSaveLocationFuture = ThreadManager.getInstance().asyncWithFixDelay(mCustomTimer, SAVE_LOC_INTERVAL, SAVE_LOC_INTERVAL, TimeUnit.MILLISECONDS);
    }

    private void stopLocStorageTimerTask() {
        if (mSaveLocationFuture != null) {
            ThreadManager.getInstance().cancelDelayRun(mSaveLocationFuture);
            mCustomTimer = null;
        }
    }

    @Override
    public boolean handleMessage(@NonNull Message msg) {
        switch (msg.what) {
            case MSG_LOC_DR_INFO_UPDATE: {
                for (IPositionAdapterCallback callback : callbacks) {
                    callback.onLocationInfo(locInfoBean);
                }
            }
            break;
            case MSG_LOC_STATUS_UPDATE: {
                for (IPositionAdapterCallback callback : callbacks) {
                    callback.onGpsSatellitesChanged(mIsLocSuccess);
                }
                if (!mIsLocSuccess) {
                    ToastUtils.Companion.getInstance().showCustomToastView(AppContext.getInstance().getMContext().getString(com.android.utils.R.string.navi_loc_filed));
                }
            }
            break;
            case MSG_LOC_GNSS_INFO_UPDATE: {
//                for (OnLocGnssInfoUpdateListener listener : mGnssInfoUpdateListeners) {
//                    listener.onLocGnssInfo((LocGnss) msg.obj);
//                }
            }
            break;
            case MSG_SEND_DR:
                for (IPositionAdapterCallback callback : callbacks) {
                    callback.onDrInfo(drInfo);
                }
                if (mH.hasMessages(MSG_SEND_DR)) {
                    mH.removeMessages(MSG_SEND_DR);
                }
                Message message = new Message();
                message.what = MSG_SEND_DR;
                mH.sendMessageDelayed(message, 1000);
                break;
            default:
                break;
        }
        return false;
    }

    public void updateSdkLocStatus(boolean isLocSuccess) {
        if (!checkValid() || mIsLocSuccess == isLocSuccess) {
            return;
        }
        mIsLocSuccess = isLocSuccess;
        if (mH.hasMessages(MSG_LOC_STATUS_UPDATE)) {
            mH.removeMessages(MSG_LOC_STATUS_UPDATE);
        }
        mH.sendEmptyMessage(MSG_LOC_STATUS_UPDATE);
    }

    public void onGearChanged(int gear) {
        if (mGear == gear) {
            return;
        }
        Logger.i(TAG, "saveLocStorage onGearChanged gear =" + gear);
        if (gear == PositionConstant.GearType.GEAR_PARK && System.currentTimeMillis() - mLastSaveTimes > 200) {
            mLastSaveTimes = System.currentTimeMillis();
            Logger.i(TAG, "saveLocStorage on GEAR_P");
            saveLocStorage();
        }
        mGear = gear;
    }

    /**
     * D
     * 初始化位置信息
     */
    private void initLocation() {
        locInfoBean = new LocInfoBean();
        locInfoBean.setLongitude(SettingConstant.DEFAULT_LON_SH);
        locInfoBean.setLatitude(SettingConstant.DEFAULT_LAT_SH);
        locInfoBean.setAltitude(SettingConstant.DEFAULT_ALT_SH);
    }

    //更新位置信息
    private void updateLocation(LocInfoBean location, LocInfo locInfo) {
        if (null != locInfo) {
            if (locInfo.sourType == GPS_PROVIDER_SOURCE_TYPE || locInfo.sourType == NETWORK_PROVIDER_SOURCE_TYPE) {
                if (locInfo.matchInfoCnt > 0 && locInfo.matchInfo != null && !locInfo.matchInfo.isEmpty()) {
                    LocMatchInfo matchInfo = locInfo.matchInfo.get(0);
                    // LocInfo.gpsPos为原始GPS信号转GCJ02坐标后的插针结果，不能直接使用
                    //locInfoBean.setLongitude(bean.gpsPos.lon);
                    //locInfoBean.setLatitude(bean.gpsPos.lat);
                    // locInfo.matchInfo.get(0).stPos是地图匹配后的绑路坐标，可用于导航、巡航等业务
                    Coord3DDouble stPos = locInfo.matchInfo.get(0).stPos;
                    location.setLongitude(stPos.lon);
                    location.setLatitude(stPos.lat);

                    location.setOwnership(matchInfo.ownership);
                    location.setProvider(selectProvider());
                    location.setBearing(locInfo.gpsCourse);
                    location.setSpeed(locInfo.speed);
                    location.setAccuracy(locInfo.posAcc);
                    location.setGpsTickCount(locInfo.tickTime.longValue());
                    location.setSysTickCount(locInfo.inputTickTime.longValue());
                    location.setVaccuracy(locInfo.altAcc);
                    location.setType(locInfo.sourType);
                    location.setRoadId(locInfo.roadId);
                    location.setLinkType(locInfo.matchInfo.get(0).linkType);
                    location.setCourse(locInfo.matchInfo.get(0).course);
                }
            }
        }
    }

    private void initGpsStatusChecker() {
        mGpsStatusChecker = new GpsStatusChecker();
        mGpsStatusChecker.setTimeOutListener(this);
        mGpsStatusChecker.doCount();
        mGpsStatusChecker.start();
    }

    private void uninitGpsStatusChecker() {
        if (mGpsStatusChecker != null) {
            mGpsStatusChecker.clearCount();
            mGpsStatusChecker.cancel();
            mGpsStatusChecker.setTimeOutListener(null);
            mGpsStatusChecker = null;
        }
    }

    public void onLocAnalysisResult(@PositionConstant.DRDebugEvent int infoType, String s) {
        for (IPositionAdapterCallback callback : callbacks) {
            callback.onLocAnalysisResult(infoType, s);
        }
    }

    public void locationLogSwitch(boolean isOpen) {
        if (mAutoNaviPosLogEnable != isOpen) {
            if (mPosService != null) {
                Logger.d(TAG, "locationLogSwitch：" + isOpen);
                ToastUtils.Companion.getInstance().showCustomToastView(isOpen ?
                        AppContext.getInstance().getMContext().getString(com.android.utils.R.string.navi_open_loc_log) :
                        AppContext.getInstance().getMContext().getString(com.android.utils.R.string.navi_close_loc_log));
                mPosService.signalRecordSwitch(isOpen, LocationFuncSwitch.getLocLogConf());
            }
        }
        this.mAutoNaviPosLogEnable = isOpen;
    }

    @Override
    public void onGraspRoadResult(GraspRoadResult graspRoadResult) {
        if(ConvertUtils.isEmpty(callbacks)) return;
        L2NaviBean.VehiclePositionBean vehiclePosition = new L2NaviBean.VehiclePositionBean();
        vehiclePosition.setLocationLinkOffset(ConvertUtils.double2int(graspRoadResult.offset, 0));
        vehiclePosition.setRoadClass(graspRoadResult.roadClass); // 当前自车所在道路等级
        vehiclePosition.setLocationLongitude(locInfoBean.getLongitude()); // 自车经度坐标（在sd route上的）
        vehiclePosition.setLocationLatitude(locInfoBean.getLatitude()); // 自车纬度坐标（在sd route上的）
        vehiclePosition.setRoadOwnership(locInfoBean.getOwnership()); // 自车所在道路所有权
        for (IPositionAdapterCallback callback : callbacks) {
            callback.onGraspRouteResult(vehiclePosition);
        }
    }

    public List<IPositionAdapterCallback> getCallBack(){
        return callbacks;
    }
}
