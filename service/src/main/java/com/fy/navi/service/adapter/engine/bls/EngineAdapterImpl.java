package com.fy.navi.service.adapter.engine.bls;

import android.annotation.SuppressLint;
import android.provider.Settings;
import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
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
import com.fy.navi.service.define.user.account.AccountProfileInfo;
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.ui.BaseApplication;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;

/**
 * TODO说明
 *
 * @author lvww
 * @version $Revision.2024/11/24$
 */
public class EngineAdapterImpl implements IEngineApi {
    private static final String TAG = MapDefaultFinalTag.ENGINE_SERVICE_TAG;
    private final List<EngineObserver> mEngineObserverList;
    private static final String ERR_MSG = "Error Massage : ";
    // TODO: 配置父子渠道包，每次更新aar包时都要核实一下
    private final String mChanelName = "C13953968867";

    static {
        Logger.i(TAG, "load gbl xxx.so");
        SdkSoLoadUtils.loadLibrary();
    }

    public EngineAdapterImpl() {
        mEngineObserverList = new ArrayList<>();
    }

    @Override
    public void addInitEnginObserver(final EngineObserver observer) {
        if (ConvertUtils.isContain(mEngineObserverList, observer)) {
            return;
        }
        mEngineObserverList.add(observer);
    }

    @Override
    public void checkSdkLimit() {
        checkUatMapLimit();
    }

    @Override
    public void initBaseLibs() {

        final int initBaseLibsResult = initEngineParam();
        if (!ConvertUtils.equals(0, initBaseLibsResult)) {
            onEngineObserver(10004);
        } else {
            onEngineObserver(10005);
        }
    }

    @Override
    public void initBL() {
        final int overDue = isOverdue();
        if (!ConvertUtils.equals(0, overDue)) return;
        final int sdkResultCode = initSDKParam();
        if (!ConvertUtils.equals(0, sdkResultCode)) {
            onEngineObserver(10006);
            return;
        }
        onEngineObserver(0);
    }

    @Override
    public void switchLog(final GaodeLogLevel logLevel) {
        /* 场景一：关闭日志 */
//        ServiceMgr.getServiceMgrInstance().switchLog(ALCLogLevel.LogLevelNone);
//        ServiceMgr.getServiceMgrInstance().setGroupMask(ALCGroup.GROUP_MASK_NULL);
        /* 场景二：低频Log */
        if (logLevel.getCode() == GaodeLogLevel.LOG_NONE.getCode()) {
            ServiceMgr.getServiceMgrInstance().switchLog(ALCLogLevel.LogLevelNone);
        } else if (logLevel.getCode() == GaodeLogLevel.LOG_DEBUG.getCode()) {
            ServiceMgr.getServiceMgrInstance().switchLog(ALCLogLevel.LogLevelDebug);
        } else if (logLevel.getCode() == GaodeLogLevel.LOG_INFO.getCode()) {
            ServiceMgr.getServiceMgrInstance().switchLog(ALCLogLevel.LogLevelInfo);
        } else if (logLevel.getCode() == GaodeLogLevel.LOG_WARN.getCode()) {
            ServiceMgr.getServiceMgrInstance().switchLog(ALCLogLevel.LogLevelWarn);
        } else if (logLevel.getCode() == GaodeLogLevel.LOG_ERROR.getCode()) {
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
    public void unInit() {
        ServiceMgr.getServiceMgrInstance().unInitBL();
        ServiceMgr.getServiceMgrInstance().unInitBaseLibs();
    }

    @Override
    public int engineID(final MapType mapId) {
        switch (mapId) {
            case MAIN_SCREEN_MAIN_MAP:
                return MapEngineID.MapEngineIdMain;
            case LAUNCHER_DESK_MAP:
                return MapEngineID.MapEngineIdEx1;
            case LAUNCHER_WIDGET_MAP:
                return MapEngineID.MapEngineIdEx2;
            case HUD_MAP:
                return MapEngineID.MapEngineIdEx3;
            case CLUSTER_MAP:
                return MapEngineID.MapEngineIdEx4;
            default:
                break;
        }
        return MapEngineID.MapEngineIdInvalid;
    }

    @Override
    public int eagleEyeEngineID(final MapType mapId) {
        switch (mapId) {
            case MAIN_SCREEN_MAIN_MAP:
                return MapEngineID.MapEngineIdMainEagleEye;
            case LAUNCHER_DESK_MAP:
                return MapEngineID.MapEngineIdEx1EagleEye;
            case LAUNCHER_WIDGET_MAP:
                return MapEngineID.MapEngineIdEx2EagleEye;
            case HUD_MAP:
                return MapEngineID.MapEngineIdEx3EagleEye;
            case CLUSTER_MAP:
                return MapEngineID.MapEngineIdEx4EagleEye;
            default:
                break;
        }
        return MapEngineID.MapEngineIdInvalid;
    }

    @Override
    public int mapDeviceID(final MapType mapId) {
        switch (mapId) {
            case MAIN_SCREEN_MAIN_MAP:
                return EGLDeviceID.EGLDeviceIDDefault;
            case LAUNCHER_DESK_MAP:
                return EGLDeviceID.EGLDeviceIDExternal1;
            case LAUNCHER_WIDGET_MAP:
                return EGLDeviceID.EGLDeviceIDExternal2;
            case HUD_MAP:
                return EGLDeviceID.EGLDeviceIDExternal3;
            case CLUSTER_MAP:
                return EGLDeviceID.EGLDeviceIDExternal4;
            default:
                break;
        }
        return EGLDeviceID.EGLDeviceIDDefault;
    }


    @Override
    public String styleBlPath(final MapType mapId) {
        return GBLCacheFilePath.BLS_ASSETS_LAYER_PATH + "style_bl.json";
    }

    @Override
    public String getChanelName() {
        return mChanelName;
    }

    /**
     * 是否过期
     *
     * @return 错误码
     */
    private int isOverdue() {
        //单位微秒
        final long limitTime = ServiceMgr.getServiceMgrInstance().getSdkLimitTimeUTC();
        if (limitTime == 0) {
            return onEngineObserver(10003);
        }
        final long limitTimeMillis = limitTime / 1000;
        final long currentTime = System.currentTimeMillis();
        Logger.d(TAG, "====limitTimeMillis = {?}， currentTime = {?}", limitTimeMillis, currentTime);
        if (limitTimeMillis < currentTime) {
            return onEngineObserver(10002);
        }
        return 0;
    }

    /**
     * 初始化BaseLib参数
     *
     * @return 初始化结果
     */
    private int initEngineParam() {
        final BaseInitParam baseInitParam = new BaseInitParam();
        baseInitParam.logFileName = "amap.android.";
        if (null == FileUtils.SD_APP_PATH) {
            Logger.i(TAG, "SD_APP_PATH is null");
            FileUtils.SD_PATH = AppContext.getInstance().getMContext().
                    getExternalFilesDir(null) + File.separator;
            Logger.i(TAG, "FileUtils.SD_PATH = " + FileUtils.SD_PATH + " myPid = " +
                    BaseApplication.getMyPid());
        }
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
        baseInitParam.channelName = mChanelName;
        baseInitParam.setIPlatformInterface(PLAT_FORM_INTERFACE);
        if (FileUtils.getInstance().checkFileDir(GBLCacheFilePath.GM_LOG_ROOT_PATH) &&
                FileUtils.getInstance().setFullPermissions(GBLCacheFilePath.GM_LOG_ROOT_PATH)) {
            baseInitParam.logPath = GBLCacheFilePath.BLS_LOG_DATA;
        } else {
            baseInitParam.logPath = GBLCacheFilePath.BLS_LOG;
            FileUtils.getInstance().setFullPermissions(GBLCacheFilePath.GBL_MAP);
        }
        boolean dir = FileUtils.getInstance().createDir(baseInitParam.logPath);
        Logger.i(TAG, "create log file path", baseInitParam.logPath + ",dir：" + dir);
        final int result = ServiceMgr.getServiceMgrInstance().initBaseLibs(baseInitParam, AppContext.getInstance().getMApplication());
        Logger.i(TAG, "initEngineParam result", result);
        return result;
    }

    /**
     * 初始化BL参数
     *
     * @return 初始化结果
     */
    private int initSDKParam() {
        final BLInitParam blInitParam = new BLInitParam();
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
        final int result = ServiceMgr.getServiceMgrInstance().initBL(blInitParam, AppContext.getInstance().getMApplication());
        Logger.i(TAG, "initSDKParam", result);
        return result;
    }

    /**
     * 引擎初始化结果回调.
     *
     * @param code !0 失败/0 成功
     *             <p>Service只返回错误码，是否弹窗提醒由HMI仲裁
     * @return code
     */
    private int onEngineObserver(final int code) {
        for (EngineObserver observer : mEngineObserverList) {
            if (ConvertUtils.equals(0, code)) {
                observer.onInitEngineSuccess();
            } else if(ConvertUtils.equals(10005, code)){
                observer.onInitBaseLibSuccess();
            }else {
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
        final long mSdkLimitTimeUTC = ServiceMgr.getServiceMgrInstance().getSdkLimitTimeUTC();
        Logger.i(TAG, "sdkLimitTimeUTC = " + mSdkLimitTimeUTC);
        //int activateCode;
        //获取SDK的限制时间（UTC），如该版本为激活包等没有配置有效时间的情况，则返回0
        if (mSdkLimitTimeUTC != 0) {
            //获取的是微秒数，需要转换成毫秒
            final long sdkLimitTimeUTC = (long) (mSdkLimitTimeUTC * 0.001);
            if (sdkLimitTimeUTC != 0) {
                //当前时间 毫秒
                final long currentTimeMillis = System.currentTimeMillis();
                //14天限制
                final long limitTime = 7L * 24 * 60 * 60 * 1000;
                Logger.i(TAG, "sdkLimitTimeUTC = " + sdkLimitTimeUTC + ",currentTimeMillis = " + currentTimeMillis + "," + limitTime);
                if ((sdkLimitTimeUTC - currentTimeMillis) <= limitTime) {
                    final SimpleDateFormat fds = new SimpleDateFormat("MM月dd日", Locale.getDefault());
                    final String mmDdByDate = fds.format(new Date(sdkLimitTimeUTC));
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

    private static final IPlatformInterface PLAT_FORM_INTERFACE = new IPlatformInterface() {

        @Override
        public float getDensity(final int i) {
            return ResourceUtils.Companion.getInstance().getDisplayMetrics().density;
        }

        @Override
        public int getDensityDpi(final int i) {
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
        public boolean getAosNetworkParam(final ArrayList<KeyValue> arrayList) {

            @SuppressLint("HardwareIds") final String androidId = Settings.Secure.getString(
                    AppContext.getInstance().getMContext().getContentResolver(),
                    Settings.Secure.ANDROID_ID
            );
            /**< diu 设备唯一号,android--imei, ios--IDFV必须设置,从系统获取，若不足32位用零补齐 */
            arrayList.add(new KeyValue("diu", androidId));
            /**< client_network_class   获取当前网络状态 2G=1；3G=2；4G=3；wifi=4 ；*/
            arrayList.add(new KeyValue("client_network_class", "4"));
            /**< uid 如果已登录，需要传入，通过用户登录接口由服务端下发 */
            final String uid = getUid();
            if (!uid.isEmpty()) {
                arrayList.add(new KeyValue("uid", uid));
            }

            return true;
        }

        @Override
        public String amapEncode(final byte[] bytes) {
            return "";
        }

        @Override
        public String amapEncodeBinary(final byte[] bytes) {
            return "";
        }

        @Override
        public String amapDecode(final byte[] bytes) {
            return "";
        }

        @Override
        public boolean getAosSign(final String s, final String[] strings) {
            return false;
        }
    };

    /**
     * 获取uid
     *
     * @return uid
     */
    public static String getUid() {
        String uid = "";
        final CommonManager commonManager = CommonManager.getInstance();
        commonManager.init();
        AccountProfileInfo info = new AccountProfileInfo();
        final String valueJson = commonManager.getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        Logger.i("getUserInfo valueJson = " + valueJson);
        if (!TextUtils.isEmpty(valueJson)) {
            info = GsonUtils.fromJson(valueJson, AccountProfileInfo.class);
            uid = info.getUid();
        }
        return uid;
    }
}