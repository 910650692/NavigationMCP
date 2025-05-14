package com.fy.navi.service.adapter.activate.bls;

import android.os.SystemProperties;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.DevicesIdUtil;
import com.android.utils.OkHttpUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.activation.ActivationModule;
import com.autonavi.gbl.activation.model.ActivateReturnParam;
import com.autonavi.gbl.activation.model.ActivationInitParam;
import com.autonavi.gbl.activation.observer.INetActivateObserver;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.activate.cloudpatac.request.UuidReq;
import com.fy.navi.service.adapter.activate.cloudpatac.response.AppKeyResponse;
import com.fy.navi.service.adapter.activate.cloudpatac.api.NetQueryRepository;
import com.fy.navi.service.adapter.activate.cloudpatac.request.AppKeyReq;
import com.fy.navi.service.adapter.activate.cloudpatac.response.CheckOrderResponse;
import com.fy.navi.service.adapter.activate.cloudpatac.response.UuidResponse;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.patac.netlib.callback.NetDisposableObserver;
import com.patac.netlib.exception.ApiException;
import com.patac.netlib.factory.NetPkiFactory;
import com.patac.netlib.utils.NetConfigUtils;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.Arrays;
import java.util.HashMap;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import io.reactivex.Observable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.schedulers.Schedulers;

public final class ActivationManager {
    private static final String TAG = MapDefaultFinalTag.ACTIVATE_SERVICE_TAG;

    private static final String TEST_APP_ID = "SELF_DEVELOPED_MAP";
    private final static String RELEASE_STATUS = "ro.patac.production";
    private static String DEVICES_ID = "";
    private static String SYS_VERSION = "";
    private static String API_VERSION = "";
    private static String APP_KEY = "";
    private static String AUTH_CODE = "";
    private static String UUID = "";

    private boolean mFlagForTest = false;

    private IActivateHelper mActivateListener;
    private final ActivationModule mActivationService;
    private boolean mIsInit = false;


    private final INetActivateObserver mNetActivateObserver = new INetActivateObserver() {
        @Override
        public void onNetActivateResponse(final int returnCode) {
            //网络激活结果处理
            Logger.i(TAG, "网络激活返回码 = " + returnCode);
            // mActivateListener.onNetActivated(ConvertUtils.equals(0, returnCode));
        }
    };

    private ActivationManager() {
        mActivationService = ActivationModule.getInstance();
        DEVICES_ID = DevicesIdUtil.getInstance().getDeviceId();
        SYS_VERSION = "1.0";
        API_VERSION = "1.0";
        Logger.d(TAG, "ActivationManager: devicesId = " + DEVICES_ID);
        Logger.d(TAG, "                  sysVersion = " + SYS_VERSION);

        final int releaseStatus = SystemProperties.getInt(RELEASE_STATUS, 0);
        Logger.d(TAG, "当前车机环境 : "
                + releaseStatus + " : " + (releaseStatus == 1 ? "生产环境" : "测试环境"));
    }

    /**
     * 反初始化
     */
    public void unInit() {
        if (!mIsInit) {
            return;
        }
        Logger.d(TAG, "unInit: ");
        mActivationService.setNetActivateObserver(null);
        mActivationService.unInit();
        mActivateListener = null;
    }

    /**
     * 添加手动激活成功回调
     *
     * @param listener listener
     */
    public void addManualActivateListener(final IActivateHelper listener) {
        mActivateListener = listener;
    }

    public static ActivationManager getInstance() {
        return Helper.INSTANCE;
    }

    private static final class Helper {
        private static final ActivationManager INSTANCE = new ActivationManager();
    }

    /**
     * 获取三方UUID
     */
    public void getThirdPartyUUID() {
        Logger.d(TAG, "getThirdPartyUUID: ");
        genAppKey();
        //mActivateListener.onUUIDGet("1");
    }

    /**
     * appKey生成
     */
    private void genAppKey() {
        Logger.d(TAG, "genAppKey: ");

        if (!ConvertUtils.isEmpty(APP_KEY)) {
            Logger.d(TAG, "APP_KEY静态变量不为空 = " + APP_KEY);
            NetPkiFactory.getInstance().saveAppSecurity(APP_KEY);
            mFlagForTest = true;
            postUUID();
            return;
        }

        if (!ConvertUtils.isEmpty(AccountPackage.getInstance().getAppKey())) {
            APP_KEY = AccountPackage.getInstance().getAppKey();
            Logger.d(TAG, "APP_KEY静态变量为空，数据库存有APP_KEY = " + APP_KEY);
            NetPkiFactory.getInstance().saveAppSecurity(APP_KEY);
            mFlagForTest = true;
            postUUID();
            return;
        }

        final AppKeyReq req = new AppKeyReq(API_VERSION);

        final Observable<AppKeyResponse> observable = NetQueryRepository.getInstance().queryAppKey(req);

        observable.subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new NetDisposableObserver<AppKeyResponse>() {
                    @Override
                    public void onSuccess(final AppKeyResponse appKeyBean) {
                        Logger.d(TAG, appKeyBean.toString());
                        APP_KEY = appKeyBean.getAppKey();
                        //AccountPackage.getInstance().saveAppKey(appKeyBean.getAppKey());
                        //postUUID();
                        mFlagForTest = true;
                    }

                    @Override
                    public void onFailed(final ApiException e) {
                        Logger.d(TAG, "Exception code : " + e.getCode());
                        Logger.d(TAG, "Exception msg : " + e.getMessage());
                    }
                });


    }

    /**
     * 网络请求uuid
     */
    private void postUUID() {
        Logger.d(TAG, "postUUID: ");

        if (!ConvertUtils.isEmpty(UUID)) {
            Logger.d(TAG, "UUID静态变量不为空 = " + UUID);
            mActivateListener.onUUIDGet(UUID);
            return;
        }

        if (!ConvertUtils.isEmpty(AccountPackage.getInstance().getUuid())) {
            UUID = AccountPackage.getInstance().getUuid();
            Logger.d(TAG, "UUID静态变量为空，数据库存有UUID = " + UUID);
            mActivateListener.onUUIDGet(UUID);
            return;
        }

        final UuidReq uuidReq = new UuidReq(API_VERSION, TEST_APP_ID, SYS_VERSION, DEVICES_ID);
        NetConfigUtils.getInstance().setDeviceId(DEVICES_ID);
        Logger.d(TAG, "uuid req : " + uuidReq.toString());
        final Observable<UuidResponse> observable = NetQueryRepository.getInstance().queryUuid(uuidReq);

        observable.subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new NetDisposableObserver<UuidResponse>() {
                    @Override
                    public void onSuccess(final UuidResponse uuidBean) {
                        Logger.d(TAG, uuidBean.toString());
                        UUID = uuidBean.getUuid();
                        mActivateListener.onUUIDGet(uuidBean.getUuid());
                    }

                    @Override
                    public void onFailed(final ApiException e) {
                        Logger.d(TAG, "Exception code : " + e.getCode());
                        Logger.d(TAG, "Exception msg : " + e.getMessage());
                    }
                });
    }

    /**
     * 初始化激活服务
     *
     * @param uuid uuid
     * @return 是否成功
     */
    public boolean initActivationService(final String uuid) {
        Logger.d(TAG, "initActivationService: ");

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
        actInitParam.szDeviceID = uuid;
        // 激活文件保存路径
        actInitParam.szUserDataFileDir = GBLCacheFilePath.ACTIVATE_USER_DATA;

        final int initResult = mActivationService.init(actInitParam);
        Logger.i(TAG, "initActivateParam: initResult = " + initResult);
        final boolean initSuccess = ConvertUtils.equals(0, initResult);
        mIsInit = initSuccess;
        return initSuccess;
    }

    /**
     * 检查激活状态
     *
     * @return 是否成功
     */
    public boolean checkActivationStatus() {
        if (mActivationService == null) {
            Logger.d(TAG, "mActivationService == null");
            return false;
        }
        if (!mIsInit) {
            Logger.d(TAG, "mActivationService 未初始化");
            return false;
        }
        final int activateStatus = mActivationService.getActivateStatus();
        Logger.d(TAG, "激活状态码 = " + activateStatus);
        //return ConvertUtils.equals(0, activateStatus);
        return mFlagForTest;
    }

    /**
     * 云对云下单
     */
    public void createCloudOrder() {
        Logger.d(TAG, "createCloudOrder");
        mActivateListener.onOrderCreated(true);
        // 调用三方下单接口
    }


    /**
     * 带重试的订单激活状态查询
     *
     * @return 返回是否成功
     */
    public boolean pollOrderStatusWithRetry() {
        final ScheduledExecutorService executor = Executors.newScheduledThreadPool(1);
        final CompletableFuture<Boolean> future = new CompletableFuture<>();
        final AtomicInteger retryCount = new AtomicInteger(0);
        final int maxRetries = 3;
        final long[] delays = {
                AutoMapConstant.DELAY_MINUTE * 2,
                AutoMapConstant.DELAY_MINUTE * 5,
                AutoMapConstant.DELAY_MINUTE * 5
        };
        final AtomicReference<Runnable> taskRef = new AtomicReference<>();

        final Runnable task = new Runnable() {
            @Override
            public void run() {
                if (future.isDone()) {
                    return;
                }
                try {
                    final int currentCount = retryCount.incrementAndGet();
                    Logger.d(TAG, "轮询次数 : " + currentCount);
                    if (currentCount >= maxRetries) {
                        future.complete(true);
                        executor.shutdownNow();
                    } else {
                        executor.schedule(taskRef.get(), delays[currentCount], TimeUnit.SECONDS);
                    }

                    getInstance().checkOrderStatus(new NetDisposableObserver<CheckOrderResponse>() {
                        @Override
                        public void onSuccess(final CheckOrderResponse appKeyBean) {
                            Logger.d(TAG, "checkOrderStatus success");
                            manualActivate("", "");
                            future.complete(true);
                            executor.shutdownNow();
                        }

                        @Override
                        public void onFailed(final ApiException e) {
                            Logger.d(TAG, "checkOrderStatus failed");
                            Logger.d(TAG, "Exception code : " + e.getCode());
                            Logger.d(TAG, "Exception msg : " + e.getMessage());

                            final int currentCount = retryCount.incrementAndGet();
                            Logger.d(TAG, "轮询次数 : " + currentCount);
                            if (currentCount > maxRetries) {
                                future.complete(false);
                                executor.shutdownNow();
                            } else {
                                executor.schedule(taskRef.get(), delays[currentCount], TimeUnit.SECONDS);
                            }
                        }
                    });

                } catch (NullPointerException | IllegalArgumentException e) {
                    future.completeExceptionally(e);
                    executor.shutdownNow();
                }
            }
        };
        taskRef.set(task);
        executor.schedule(task, delays[0], TimeUnit.SECONDS);

        try {
            // 总超时 = 所有可能延迟之和 + 缓冲时间（例如15分钟）
            final long totalTimeout = Arrays.stream(delays).sum() + 1; // 2+5+5 +1=13分钟
            return future.get(totalTimeout, TimeUnit.SECONDS);
        } catch (CancellationException e) {
            executor.shutdownNow();
            Logger.e(TAG, "CancellationException");
            return false;
        } catch (TimeoutException e) {
            executor.shutdownNow();
            Logger.e(TAG, "订单状态轮询总超时");
            return false;
        } catch (ExecutionException e) {
            Logger.e(TAG, "轮询异常: " + e.getCause());
            return false;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            Logger.e(TAG, "轮询被中断");
            return false;
        } finally {
            executor.shutdown();
        }
    }

    /**
     * 查询订单状态
     *
     * @param netDisposableObserver 网络结果回调
     */
    public void checkOrderStatus(final NetDisposableObserver<CheckOrderResponse> netDisposableObserver) {
        Logger.d(TAG, "checkOrderStatus");

    }

    /**
     * 网络激活
     */
    public void netActivate() {
        //++NET_FAILED_COUNT;
        //final String hardWareCode = "0000000000"; // 默认10个0
        //final int netActivateResult = mActivationService.netActivate(hardWareCode);
        //Logger.i(TAG, "netActivateResult = " + netActivateResult + "; failed count : " + NET_FAILED_COUNT);
        mActivateListener.onNetActivated(false);
    }
/*
这是激活需要的参数信息
设备号：22A5E69D7783D0A06C62BB7BE9A73ADA(001042)

序列号：PETAW9KTKQS79GZQBCXA984B

激活码：UR32YH4SP4S9SQ57SJZ9ZUK9

渠道号:C13953968867
 */

    /**
     * 手动激活
     *
     * @param userCode  序列号
     * @param loginCode 激活码
     */
    public void manualActivate(final String userCode, final String loginCode) {
        Logger.d(TAG, "userCode = " + userCode + "; loginCode = " + loginCode);
        if (mActivationService == null) {
            Logger.d(TAG, "mActivationService == null");
            return;
        }
        if (!mIsInit) {
            Logger.d(TAG, "mActivationService 未初始化");
            return;
        }
        final ActivateReturnParam activateReturnParam = mActivationService.manualActivate(userCode, loginCode);
        if (activateReturnParam == null) {
            Logger.d(TAG, "activateReturnParam == null");
            return;
        }
        Logger.d(TAG, "activateReturnParam.iErrorCode = " + activateReturnParam.iErrorCode);
        mActivateListener.onManualActivated(ConvertUtils.equals(0, activateReturnParam.iErrorCode));
    }

    public interface IActivateHelper {

        /**
         * uuid获取后回调
         *
         * @param uuid uuid
         */
        void onUUIDGet(final String uuid);

        /**
         * 手动激活结果回调给impl
         *
         * @param isSuccess 是否成功
         */
        void onManualActivated(final boolean isSuccess);

        /**
         * 网络激活回调impl
         *
         * @param isSuccess 是否成功
         */
        void onNetActivated(final boolean isSuccess);

        /**
         * 开始下单回调给impl
         *
         * @param isSuccess 是否成功
         */
        void onOrderCreated(final boolean isSuccess);
    }
}