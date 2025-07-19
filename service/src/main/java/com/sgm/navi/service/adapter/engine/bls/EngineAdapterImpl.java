package com.sgm.navi.service.adapter.engine.bls;

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
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.servicemanager.model.ALCGroup;
import com.autonavi.gbl.servicemanager.model.ALCLogLevel;
import com.autonavi.gbl.servicemanager.model.BLInitParam;
import com.autonavi.gbl.servicemanager.model.BaseInitParam;
import com.autonavi.gbl.servicemanager.model.ServiceManagerEnum;
import com.autonavi.gbl.util.model.KeyValue;
import com.autonavi.gbl.util.observer.IPlatformInterface;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.calibration.CalibrationAdapter;
import com.sgm.navi.service.adapter.engine.EngineObserver;
import com.sgm.navi.service.adapter.engine.IEngineApi;
import com.sgm.navi.service.define.code.CodeManager;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.engine.GaodeLogLevel;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.user.account.AccountProfileInfo;
import com.sgm.navi.service.greendao.CommonManager;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * TODO说明
 *
 * @author lvww
 * @version $Revision.2024/11/24$
 */
public class EngineAdapterImpl implements IEngineApi {
    private static final String TAG = MapDefaultFinalTag.ENGINE_SERVICE_TAG;
    private List<EngineObserver> mEngineObserverList;
    // TODO: 配置父子渠道包，每次更新aar包时都要核实一下
    private static final String mChanelName = "C13953968867";
    private static final String mGmcL2ChanelName = "C13953984967";
    private final AtomicBoolean mIsEngineInit = new AtomicBoolean(false);

    public EngineAdapterImpl() {
        mEngineObserverList = new ArrayList<>();
    }

    @Override
    public void loadLibrary() {
        try {
            mIsEngineInit.set(false);
            Logger.i(TAG, "load gbl xxx.so", "mIsEngineInit : ", mIsEngineInit);
            System.loadLibrary("Gbl");
            onEngineObserver(10008);
        } catch (UnsatisfiedLinkError e) {
            onEngineObserver(10007);
        }
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
        if (mIsEngineInit.get()) {
            Logger.i(TAG, "引擎已经初始化过，无需重复初始化 ", mIsEngineInit);
            return;
        }
        final int overDue = isOverdue();
        if (!ConvertUtils.equals(0, overDue)) return;
        final int initBaseLibsResult = initEngineParam();
        if (!ConvertUtils.equals(0, initBaseLibsResult)) {
            onEngineObserver(10004);
        } else {
            onEngineObserver(10005);
        }
    }

    @Override
    public void initBL() {
        if (mIsEngineInit.get()) {
            if (mIsEngineInit.get()) {
                Logger.i(TAG, "引擎已经初始化过，无需重复初始化 " , mIsEngineInit);
                return;
            }
            return;
        }
        final int sdkResultCode = initSDKParam();
        if (!ConvertUtils.equals(0, sdkResultCode)) {
            onEngineObserver(10006);
            return;
        }
        mIsEngineInit.set(true);
        onEngineObserver(0);
    }

    @Override
    public void switchLog(final GaodeLogLevel logLevel) {
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
    }

    @Override
    public void unInit() {
        mIsEngineInit.set(false);
        ServiceMgr.getServiceMgrInstance().unInitBL();
        ServiceMgr.getServiceMgrInstance().unInitBaseLibs();
        mEngineObserverList.clear();
        mEngineObserverList = null;
    }

    /*
     * 轻易不要改动 关系已对应好
     * 一个mapId对应一个主屏一个鹰眼屏幕  主屏是单数 鹰眼是双数
     * deviceId是01234...
     * */
    @Override
    public int engineID(final MapType mapId) {
        return mapId.getMapType();
    }

    /* 轻易不要改动 关系已对应好 */
    @Override
    public int eagleEyeEngineID(final MapType mapId) {
        return mapId.getMapType() + 1;
    }

    /* 轻易不要改动 关系已对应好 */
    @Override
    public int mapDeviceID(final MapType mapId) {
        return (mapId.getMapType() - 1) / 2;
    }


    @Override
    public String styleBlPath(final MapType mapId) {
        return GBLCacheFilePath.BLS_ASSETS_LAYER_PATH + "style_bl.json";
    }

    @Override
    public String getChanelName() {
        if (CalibrationAdapter.getInstance().adasConfigurationType() == 8) {
            return mGmcL2ChanelName;
        }
        return mChanelName;
    }

    @Override
    public boolean engineIsInit() {
        return mIsEngineInit.get();
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
            Logger.d(TAG, "limitTime == 0 代表没有配置有效时间 不做拦截");
//            return onEngineObserver(10003);
        } else {
            final long limitTimeMillis = limitTime / 1000;
            final long currentTime = System.currentTimeMillis();
            Logger.d(TAG, "limitTimeMillis", limitTimeMillis, "currentTime", currentTime);
            if (limitTimeMillis < currentTime) {
                return onEngineObserver(10002);
            }
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
        baseInitParam.logPath = GBLCacheFilePath.BLS_LOG;
        if (Logger.openLog) {
            Logger.d(TAG, "logPath ", baseInitParam.logPath);
        }
        baseInitParam.logLevel = ALCLogLevel.LogLevelDebug;
        /* 日志掩码 */
        baseInitParam.groupMask = ALCGroup.GROUP_MASK_ALL;
        /* 日志输出的方式是同步还是异步，默认异步 */
        baseInitParam.async = false;
        /* AutoSDK底层JNI层的日志是否打印到logcat一同输出 */
        baseInitParam.bLogcat = true;
        /* 后台服务器类型，默认用正式发布环境类型（千万不要轻易切换类型）*/
        baseInitParam.serverType = ServiceManagerEnum.AosProductionEnv;
        /* 访问后台服务的cookie存放路径 */
        baseInitParam.aosDBPath = GBLCacheFilePath.BLS_COOKIES_PATH;
        if (Logger.openLog) {
            Logger.d(TAG, "aosDBPath ", baseInitParam.aosDBPath);
        }
        //BL_SDK资源文件的原始目录，如 /android_assets/blRes/
        baseInitParam.assetPath = GBLCacheFilePath.BLS_ASSETS_PATH;
        if (Logger.openLog) {
            Logger.d(TAG, "assetPath ", baseInitParam.assetPath);
        }
        //缓存目录，存放asset导出文件或日志文件等
        baseInitParam.cachePath = GBLCacheFilePath.COPY_ASSETS_DIR;
        if (Logger.openLog) {
            Logger.d(TAG, "cachePath ", baseInitParam.cachePath);
        }
        //用户数据目录，保存用户生成数据
        baseInitParam.userDataPath = GBLCacheFilePath.USER_CACHE_PATH;
        if (Logger.openLog) {
            Logger.d(TAG, "userDataPath ", baseInitParam.userDataPath);
        }
        baseInitParam.channelName = getChanelName();
        if (Logger.openLog) {
            Logger.d(TAG, "channelName ", baseInitParam.channelName);
        }
        baseInitParam.setIPlatformInterface(PLAT_FORM_INTERFACE);
        baseInitParam.logPath = GBLCacheFilePath.BLS_LOG;
        FileUtils.getInstance().setFullPermissions(GBLCacheFilePath.GBL_MAP);
        if (Logger.openLog) {
            Logger.d(TAG, "logPath ", baseInitParam.logPath);
        }
        boolean dir = FileUtils.getInstance().createDir(baseInitParam.logPath);
        Logger.i(TAG, "create log file path", baseInitParam.logPath + ",dir：" + dir);
        final int result = ServiceMgr.getServiceMgrInstance().initBaseLibs(baseInitParam, AppCache.getInstance().getMApplication());
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
        blInitParam.dataPath.cfgFilePath = GBLCacheFilePath.GBL_MAP;
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
        final int result = ServiceMgr.getServiceMgrInstance().initBL(blInitParam, AppCache.getInstance().getMApplication());
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
            } else if (ConvertUtils.equals(10005, code)) {
                observer.onInitBaseLibSuccess();
            } else if (ConvertUtils.equals(10007, code)) {
                observer.onLoadLibraryFail(code, CodeManager.getInstance().getEngineMsg(code));
            } else if (ConvertUtils.equals(10008, code)) {
                observer.onLoadLibrarySuccess();
            } else {
                observer.onInitEngineFail(code, CodeManager.getInstance().getEngineMsg(code));
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
                        ToastUtils.Companion.getInstance().showCustomToastView("地图试用版本于在" + mmDdByDate + "到期，请尽快升级");
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
                    AppCache.getInstance().getMContext().getContentResolver(),
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
        AccountProfileInfo info;
        final String valueJson = commonManager.getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        Logger.d("getUserInfo valueJson = ", valueJson);
        if (!TextUtils.isEmpty(valueJson)) {
            info = GsonUtils.fromJson(valueJson, AccountProfileInfo.class);
            uid = info.getUid();
        }
        return uid;
    }
}