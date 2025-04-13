package com.fy.navi.service.adapter.engine.bls;

import android.annotation.SuppressLint;
import android.provider.Settings;
import android.text.TextUtils;

import androidx.work.Data;
import androidx.work.ListenableWorker;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.RunTask;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.activation.ActivationModule;
import com.autonavi.gbl.activation.model.ActivationInitParam;
import com.autonavi.gbl.activation.observer.INetActivateObserver;
import com.autonavi.gbl.map.model.EGLDeviceID;
import com.autonavi.gbl.map.model.MapEngineID;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.servicemanager.model.ALCGroup;
import com.autonavi.gbl.servicemanager.model.ALCLogLevel;
import com.autonavi.gbl.servicemanager.model.BLInitParam;
import com.autonavi.gbl.servicemanager.model.BaseInitParam;
import com.autonavi.gbl.servicemanager.model.ServiceManagerEnum;
import com.autonavi.gbl.util.model.KeyValue;
import com.autonavi.gbl.util.observer.IPlatformInterface;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.engine.EngineObserver;
import com.fy.navi.service.adapter.engine.IEngineApi;
import com.fy.navi.service.define.code.CodeManager;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.engine.GaodeLogLevel;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.user.account.AccountProfileInfo;
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.service.greendao.setting.SettingManager;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Timer;
import java.util.TimerTask;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class EngineAdapterImpl implements IEngineApi {
    private static final String TAG = MapDefaultFinalTag.ENGINE_SERVICE_TAG;
    private final List<EngineObserver> mEngineObserverList;
    private boolean isActive = false;
    // TODO: 配置父子渠道包，每次更新aar包时都要核实一下
    private final String CHANNEL_NAME = "C13953968867";
    SettingManager settingManager;
    private final ActivationModule mActivationService;
    private Timer mOrderStateCheckTimer;
    private int mOrderCheckCount = 0;
    // 首次延迟 2 分钟
    private static final long INITIAL_DELAY = 2 * AutoMapConstant.SECONDS_PER_MINUTE * AutoMapConstant.MILLISECONDS_PER_SECOND;
    // 后续延迟周期 5 分钟
    private static final long PERIOD = 5 * AutoMapConstant.SECONDS_PER_MINUTE * AutoMapConstant.MILLISECONDS_PER_SECOND;


    private final INetActivateObserver mNetActivateObserver = new INetActivateObserver() {
        @Override
        public void onNetActivateResponse(final int returnCode) {
            //网络激活结果处理
            Logger.i(TAG, "网络激活返回码 = " + returnCode);
        }
    };

    static {
        Logger.i(TAG, "load gbl xxx.so");
        SdkSoLoadUtils.loadLibrary();
    }

    public EngineAdapterImpl() {
        mEngineObserverList = new ArrayList<>();
        settingManager = SettingManager.getInstance();
        settingManager.init();
        mActivationService = ActivationModule.getInstance();
    }


    public void addInitEnginObserver(EngineObserver observer) {
        if (ConvertUtils.isContain(mEngineObserverList, observer)) return;
        mEngineObserverList.add(observer);
    }

    @Override
    public ListenableWorker.Result initEngine() {
        return ThreadManager.getInstance().supplyAsync(new RunTask<>() {
            @Override
            public ListenableWorker.Result get() {
                return startInitEngine();
            }
        }, 20);
    }

    @Override
    public void switchLog(GaodeLogLevel logLevel) {
        /* 场景一：关闭日志 */
//        ServiceMgr.getServiceMgrInstance().switchLog(ALCLogLevel.LogLevelNone);
//        ServiceMgr.getServiceMgrInstance().setGroupMask(ALCGroup.GROUP_MASK_NULL);
        /* 场景二：低频Log */
        if(logLevel.getCode() == GaodeLogLevel.LOG_NONE.getCode()){
            ServiceMgr.getServiceMgrInstance().switchLog(ALCLogLevel.LogLevelNone);
        }else if(logLevel.getCode() == GaodeLogLevel.LOG_DEBUG.getCode()){
            ServiceMgr.getServiceMgrInstance().switchLog(ALCLogLevel.LogLevelDebug);
        }else if(logLevel.getCode() == GaodeLogLevel.LOG_INFO.getCode()){
            ServiceMgr.getServiceMgrInstance().switchLog(ALCLogLevel.LogLevelInfo);
        }else if(logLevel.getCode() == GaodeLogLevel.LOG_WARN.getCode()){
            ServiceMgr.getServiceMgrInstance().switchLog(ALCLogLevel.LogLevelWarn);
        }else if(logLevel.getCode() == GaodeLogLevel.LOG_ERROR.getCode()){
            ServiceMgr.getServiceMgrInstance().switchLog(ALCLogLevel.LogLevelError);
        }
        ServiceMgr.getServiceMgrInstance().setGroupMask(ALCGroup.GROUP_MASK_BL_AE);

        /* 场景三：高频Log */
//        ServiceMgr.getServiceMgrInstance().switchLog(ALCLogLevel.LogLevelDebug);
//        ServiceMgr.getServiceMgrInstance().setGroupMask(ALCGroup.GROUP_MASK_ALL);
        /* 场景四：超高频Log */
//        ServiceMgr.getServiceMgrInstance().switchLog(ALCLogLevel.LogLevelVerbose);
//        ServiceMgr.getServiceMgrInstance().setGroupMask(ALCGroup.GROUP_MASK_ALL);
    }

    @Override
    public boolean engineStatus() {
        return isActive;
    }

    @Override
    public void unInit() {
        ServiceMgr.getServiceMgrInstance().unInitBL();
        ServiceMgr.getServiceMgrInstance().unInitBaseLibs();
        mActivationService.setNetActivateObserver(null);
        mActivationService.unInit();
        stopActivationCheck();
    }

    @Override
    public int engineID(MapType mapId) {
        switch (mapId) {
            case MAIN_SCREEN_MAIN_MAP:
                return MapEngineID.MapEngineIdMain;
            case LAUNCHER_DESK_MAP:
                return MapEngineID.MapEngineIdEx1;
            case LAUNCHER_WIDGET_MAP:
                return MapEngineID.MapEngineIdEx2;
        }
        return MapEngineID.MapEngineIdInvalid;
    }

    @Override
    public int eagleEyeEngineID(MapType mapId) {
        switch (mapId) {
            case MAIN_SCREEN_MAIN_MAP:
                return MapEngineID.MapEngineIdMainEagleEye;
            case LAUNCHER_DESK_MAP:
                return MapEngineID.MapEngineIdEx1EagleEye;
            case LAUNCHER_WIDGET_MAP:
                return MapEngineID.MapEngineIdEx2EagleEye;
        }
        return MapEngineID.MapEngineIdInvalid;
    }

    @Override
    public int mapDeviceID(MapType mapId) {
        switch (mapId) {
            case MAIN_SCREEN_MAIN_MAP:
                return EGLDeviceID.EGLDeviceIDDefault;
            case LAUNCHER_DESK_MAP:
                return EGLDeviceID.EGLDeviceIDExternal1;
            case LAUNCHER_WIDGET_MAP:
                return EGLDeviceID.EGLDeviceIDExternal2;
        }
        return EGLDeviceID.EGLDeviceIDDefault;
    }


    @Override
    public String styleBlPath(MapType mapId) {
        return GBLCacheFilePath.BLS_ASSETS_LAYER_PATH + "style_bl.json";
    }

    private ListenableWorker.Result startInitEngine() {
        checkUatMapLimit();
        if (isActive) return ListenableWorker.Result.success();
        int overDue = isOverdue();
        if (!ConvertUtils.equals(0, overDue)) {
            Data data = new Data.Builder()
                    .putInt("errorCode", overDue)
                    .putString("errorMsg", CodeManager.getEngineMsg(overDue))
                    .build();
            return ListenableWorker.Result.failure(data);
        }
        SdkSoLoadUtils.copyAssetsFiles();
        int initBaseLibsResult = initEngineParam();
        if (!ConvertUtils.equals(0, initBaseLibsResult)) {
            Data data = new Data.Builder()
                    .putInt("errorCode", 10004)
                    .putString("errorMsg", CodeManager.getEngineMsg(10004))
                    .build();
            onEngineObserver(10004);
            return ListenableWorker.Result.failure(data);
        }

//        //初始化激活服务
//        //todo获取uuid,有uuid才能初始化激活服务来查看激活状态
//        final int actInitResultCode = initActivateParam();
//        if (!ConvertUtils.equals(0, actInitResultCode)) {
//            Logger.e(TAG, "激活初始化失败");
//            final Data data = new Data.Builder()
//                    .putInt("errorCode", 10006)
//                    .putString("errorMsg", CodeManager.getEngineMsg(10006))
//                    .build();
//            onEngineObserver(10006);
//            return ListenableWorker.Result.failure(data);
//        }

        //查询激活状态
//        if (!checkActivationStatue()) {
//            //todo云对云下单
//            //检查下单状态
//            startOrderStateCheck();
//            //todo查询到下单成功后再走下面的网络激活
//            if (!ConvertUtils.equals(0, netActivate())) {
//                Logger.e(TAG, "网络激活失败");
//                final Data data = new Data.Builder()
//                        .putInt("errorCode", 10007)
//                        .putString("errorMsg", CodeManager.getEngineMsg(10007))
//                        .build();
//                onEngineObserver(10007);
//                return ListenableWorker.Result.failure(data);
//            }
//        }

        int sdkResultCode = initSDKParam();
        if (!ConvertUtils.equals(0, sdkResultCode)) {
            Data data = new Data.Builder()
                    .putInt("errorCode", 10005)
                    .putString("errorMsg", CodeManager.getEngineMsg(10005))
                    .build();
            onEngineObserver(10005);
            return ListenableWorker.Result.failure(data);
        }
        isActive = true;
        onEngineObserver(0);
        return ListenableWorker.Result.success();
    }

    private int isOverdue() {
        //单位微秒
        long limitTime = ServiceMgr.getServiceMgrInstance().getSdkLimitTimeUTC();
        if (limitTime == 0) {
            return onEngineObserver(10003);
        }
        long limitTimeMillis = limitTime / 1000;
        long currentTime = System.currentTimeMillis();
        Logger.d(TAG, "====limitTimeMillis = {?}， currentTime = {?}", limitTimeMillis, currentTime);
        if (limitTimeMillis < currentTime) {
            return onEngineObserver(10002);
        }
        return 0;
    }

    private int initEngineParam() {
        BaseInitParam baseInitParam = new BaseInitParam();
        baseInitParam.logFileName = "amap.android.";
        baseInitParam.logPath = GBLCacheFilePath.BLS_LOG;
        baseInitParam.logLevel = ALCLogLevel.LogLevelDebug;
        /* 日志掩码 */
        baseInitParam.groupMask = ALCGroup.GROUP_MASK_ALL;
        /* 日志输出的方式是同步还是异步，默认异步 */
        baseInitParam.async = false;
        /* AutoSDK底层JNI层的日志是否打印到logcat一同输出 */
        baseInitParam.bLogcat = false;
        /* 后台服务器类型，默认用正式发布环境类型（千万不要轻易切换类型）*/
        baseInitParam.serverType = ServiceManagerEnum.AosProductionEnv;
        /* 访问后台服务的cookie存放路径 */
        baseInitParam.aosDBPath = GBLCacheFilePath.BLS_COOKIES_PATH;
        //BL_SDK资源文件的原始目录，如 /android_assets/blRes/
        baseInitParam.assetPath = GBLCacheFilePath.BLS_ASSETS_PATH;
        //缓存目录，存放asset导出文件或日志文件等
        baseInitParam.cachePath = GBLCacheFilePath.COPY_ASSETS_DIR;
        //用户数据目录，保存用户生成数据
        baseInitParam.userDataPath = GBLCacheFilePath.USER_CACHE_PATH;
        baseInitParam.channelName = CHANNEL_NAME;
        settingManager.insertOrReplace(SettingController.KEY_SETTING_CHANNEL_ID, CHANNEL_NAME);
        baseInitParam.setIPlatformInterface(platformInterface);

        FileUtils.getInstance().createDir(baseInitParam.logPath);
        Logger.i(TAG, "create log file path", baseInitParam.logPath);
        int result = ServiceMgr.getServiceMgrInstance().initBaseLibs(baseInitParam, AppContext.getInstance().getMApplication());
        Logger.i(TAG, "initEngineParam result", result);
        return result;
    }

    /**
     * 初始化激活参数
     *
     * @return int
     */
    private int initActivateParam() {
        mActivationService.setNetActivateObserver(mNetActivateObserver);

        final ActivationInitParam actInitParam = new ActivationInitParam();

        // 是否检查客户编号
        actInitParam.isCheckClientNo = true;
        // 是否检查项目编号
        actInitParam.isCheckModelNo = true;
        // 是否支持 批量激活
        actInitParam.isSupportVolumeAct = false;
        //项目编号 非基础版项目传0, 内部读取配置文件的值
        actInitParam.iProjectId = 0;
        // 激活码的长度为24，设置为其他值无法激活
        actInitParam.iCodeLength = 24;
        // 设备编号 info4的uuid
        actInitParam.szDeviceID = "";
        // 激活文件保存路径
        actInitParam.szUserDataFileDir = GBLCacheFilePath.ACTIVATE_USER_DATA;

        final int initResult = mActivationService.init(actInitParam);
        Logger.i(TAG, "initActivateParam: initResult = " + initResult);
        return initResult;
    }

    /**
     * 网络激活
     * @return 激活结果
     */
    private int netActivate() {
        final String hardWareCode = "0000000000"; // 默认10个0
        final int netActivateResult = mActivationService.netActivate(hardWareCode);
        Logger.i(TAG, "netActivateResult = " + netActivateResult);
        return netActivateResult;
    }

    private int initSDKParam() {
        BLInitParam blInitParam = new BLInitParam();
        //样式、播报等相关配置存放路径
        blInitParam.dataPath.cfgFilePath = AutoMapConstant.GBL_MAP;
        //离线地图数据下载存放路径
        blInitParam.dataPath.offlinePath = GBLCacheFilePath.OFFLINE_DOWNLOAD_DIR;
        //离线地图3D数据下载存放路径
        blInitParam.dataPath.off3DDataPath = GBLCacheFilePath.OFFLINE_DOWNLOAD_DIR;
        //在线TBT、map、车道级数据缓存路径
        blInitParam.dataPath.onlinePath = GBLCacheFilePath.ONLINE_PATH;
        //车道级导航离线数据路径
        blInitParam.dataPath.lndsOfflinePath = GBLCacheFilePath.LNDS_OFF_LINE_PATH;

        FileUtils.getInstance().createDir(GBLCacheFilePath.OFFLINE_CONF_PATH);
        Logger.i(TAG, "create offline config path", GBLCacheFilePath.OFFLINE_CONF_PATH);

        FileUtils.getInstance().createDir(blInitParam.dataPath.offlinePath);
        Logger.i(TAG, "create offline file path", blInitParam.dataPath.offlinePath);

        FileUtils.getInstance().createDir(blInitParam.dataPath.onlinePath);
        Logger.i(TAG, "create online file path", blInitParam.dataPath.onlinePath);
        int result = ServiceMgr.getServiceMgrInstance().initBL(blInitParam, AppContext.getInstance().getMApplication());
        Logger.i(TAG, "initSDKParam", result);
        return result;
    }

    /**
     * 引擎初始化结果回调.
     *
     * @param code !0 失败/0 成功
     */
    private int onEngineObserver(int code) {
        for (EngineObserver observer : mEngineObserverList) {
            if (ConvertUtils.equals(0, code)) {
                observer.onInitEngineSuccess();
            } else {
                observer.onInitEngineFail(code, CodeManager.getEngineMsg(code));
            }
        }
        return code;
    }

    /**
     * 获取SDK版本号.
     *
     * @return String，SDK version.
     */
    @Override
    public String getSdkVersion() {
        return ServiceMgr.getVersion();
    }

    /**
     * 非激活版本地图，<=7天时，进行toast提醒
     */
    private void checkUatMapLimit() {
        long mSdkLimitTimeUTC = ServiceMgr.getServiceMgrInstance().getSdkLimitTimeUTC();
        Logger.i(TAG, "sdkLimitTimeUTC = " + mSdkLimitTimeUTC);
        int activateCode;
        //获取SDK的限制时间（UTC），如该版本为激活包等没有配置有效时间的情况，则返回0
        if (mSdkLimitTimeUTC != 0) {
            //获取的是微秒数，需要转换成毫秒
            long sdkLimitTimeUTC = (long) (mSdkLimitTimeUTC * 0.001);
            if (sdkLimitTimeUTC != 0) {
                //当前时间 毫秒
                long currentTimeMillis = System.currentTimeMillis();
                //14天限制
                long limitTime = 7L * 24 * 60 * 60 * 1000;
                Logger.i(TAG, "sdkLimitTimeUTC = " + sdkLimitTimeUTC + ",currentTimeMillis = " + currentTimeMillis + "," + limitTime);
                if ((sdkLimitTimeUTC - currentTimeMillis) <= limitTime) {
                    SimpleDateFormat fds = new SimpleDateFormat("MM月dd日", Locale.getDefault());
                    String mmDdByDate = fds.format(new Date(sdkLimitTimeUTC));
                    Logger.i(TAG, "sdkLimitTimeUTC mmDdByDate = " + mmDdByDate);
                    if (!TextUtils.isEmpty(mmDdByDate)) {
                        ToastUtils.Companion.getInstance().showTextLong("地图试用版本于在" + mmDdByDate + "到期，请尽快升级");
                    }
                }
            }
        }
    }

    /**
     * 获取引擎版本号.
     *
     * @return String，Engine version.
     */
    @Override
    public String getEngineVersion() {
        return ServiceMgr.getEngineVersion();
    }

    private static final IPlatformInterface platformInterface = new IPlatformInterface() {

        @Override
        public float getDensity(int i) {
            return ResourceUtils.Companion.getInstance().getDisplayMetrics().density;
        }

        @Override
        public int getDensityDpi(int i) {
            return ResourceUtils.Companion.getInstance().getDisplayMetrics().densityDpi;
        }

        @Override
        public int getNetStatus() {
            return NetWorkUtils.Companion.getInstance().getNetworkTypeValue();
        }

        @Override
        public ArrayList<KeyValue> getCdnNetworkParam() {
            return null;
        }

        @Override
        public boolean getAosNetworkParam(ArrayList<KeyValue> arrayList) {

            @SuppressLint("HardwareIds")
            String androidId = Settings.Secure.getString(
                    AppContext.getInstance().getMContext().getContentResolver(),
                    Settings.Secure.ANDROID_ID
            );
            /**< diu 设备唯一号,android--imei, ios--IDFV必须设置,从系统获取，若不足32位用零补齐 */
            arrayList.add(new KeyValue("diu", androidId));
            /**< client_network_class   获取当前网络状态 2G=1；3G=2；4G=3；wifi=4 ；*/
            arrayList.add(new KeyValue("client_network_class", "4"));
            /**< uid 如果已登录，需要传入，通过用户登录接口由服务端下发 */
            String uid = getUid();
            if (!uid.isEmpty()) {
                arrayList.add(new KeyValue("uid", uid));
            }

            return true;
        }

        @Override
        public String amapEncode(byte[] bytes) {
            return "";
        }

        @Override
        public String amapEncodeBinary(byte[] bytes) {
            return "";
        }

        @Override
        public String amapDecode(byte[] bytes) {
            return "";
        }

        @Override
        public boolean getAosSign(String s, String[] strings) {
            return false;
        }
    };

    public static String getUid() {
        String uid = "";
        CommonManager commonManager = CommonManager.getInstance();
        commonManager.init();
        AccountProfileInfo info = new AccountProfileInfo();
        String valueJson = commonManager.getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        Logger.i("getUserInfo valueJson = " + valueJson);
        if (!TextUtils.isEmpty(valueJson)) {
            info = GsonUtils.fromJson(valueJson, AccountProfileInfo.class);
            uid = info.getUid();
        }
        return uid;
    }

    /**
     * 查询下单状态
     * @return 是否成功
     */
    private boolean checkOrderState() {
        //todo 待泛亚提供查询下单状态
        Logger.e(TAG, "下单状态 = ");
        return false;
    }

    /**
     * 检查激活状态
     *
     * @return true 已激活
     */
    private boolean checkActivationStatue() {
        if (mActivationService == null) {
            Logger.e(TAG, "mActivationService == null" );
            return false;
        }
        final int activateStatus = mActivationService.getActivateStatus();
        Logger.e(TAG, "激活状态码 = " + activateStatus);
        return ConvertUtils.equals(0, activateStatus);
    }

    /**
     * 取消timer
     */
    private void cancelTimer() {
        if (mOrderStateCheckTimer != null) {
            mOrderStateCheckTimer.cancel();
            mOrderStateCheckTimer = null;
        }
    }

    /**
     * 停止任务
     */
    public void stopActivationCheck() {
        cancelTimer();
    }

    /**
     * 启动检查
     */
    private void startOrderStateCheck() {
        if (mOrderStateCheckTimer == null) {
            mOrderStateCheckTimer = new Timer();
        }
        mOrderStateCheckTimer.schedule(new OrderCheckTask(), INITIAL_DELAY, PERIOD);
    }

    /**
     * 激活查询task
     */
    private final class OrderCheckTask extends TimerTask {
        @Override
        public void run() {
            final boolean isOrdered = checkOrderState();
            ++mOrderCheckCount;
            Logger.d(TAG, "OrderCheckTask: OrderState = " + isOrdered);
            if (isOrdered) {
                cancelTimer(); // 下单成功则停止任务
            } else if (mOrderCheckCount > 3) {
                //todo失败提示
                cancelTimer();
            }
        }
    }
}
