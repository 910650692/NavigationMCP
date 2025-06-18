package com.fy.navi.activatehq;

import com.android.utils.ConvertUtils;
import com.android.utils.DevicesIdUtil;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.patacnetlib.NetQueryManager;
import com.fy.navi.patacnetlib.request.navibean.activate.CreateOrderRequest;
import com.fy.navi.patacnetlib.request.navibean.activate.QueryOrderRequest;
import com.fy.navi.patacnetlib.request.navibean.activate.UuidRequest;
import com.fy.navi.patacnetlib.response.activate.CreateOrderResponse;
import com.fy.navi.patacnetlib.response.activate.QueryOrderResponse;
import com.fy.navi.patacnetlib.response.activate.UuidResponse;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.greendao.CommonManager;
import com.gm.cn.adassdk.AdasManager;
import com.gm.cn.adassdk.UuidSubStatus;

import java.util.Arrays;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public final class ActivateHQ {
    private static final String TAG = MapDefaultFinalTag.ACTIVATE_SERVICE_TAG + "_HQ";
    private static final String TEST_APP_ID = "SELF_DEVELOPED_MAP";
    private static String DEVICES_ID;
    private static String SYS_VERSION;
    private static String API_VERSION;
    private static String UUID = "";
    private static String ORDER_ID;
    private final static String HQ = "HQ";
    private static int QUERY_ORDER_NUM = 0;
    private static final int NET_FAILED_RETRY_DELAY = 8;
    private AtomicInteger mNetFailedRetryCount = new AtomicInteger(0);

    private AdasManager mAdasManager;
    private final ScheduledExecutorService mNetRetryExecutor;

    private ActivateHQ() {
        DEVICES_ID = DevicesIdUtil.getInstance().getDeviceId();
        SYS_VERSION = "1.0";
        API_VERSION = "1.0";
        QUERY_ORDER_NUM = 0;
        mNetFailedRetryCount.set(0);
        mNetRetryExecutor = Executors.newScheduledThreadPool(1);
    }

    private static final class SingleHolder {
        public static final ActivateHQ INSTANCE = new ActivateHQ();
    }

    /**
     * 获取实例
     *
     * @return ActivateHQ
     */
    public static ActivateHQ getInstance() {
        return SingleHolder.INSTANCE;
    }

    /**
     * 初始化adasManager，同时也是hq激活接口
     *
     * @param manager adasManager
     */
    public void init(final AdasManager manager) {
        this.mAdasManager = manager;
        //startActivate();
    }

    /**
     * 开始激活
     */
    private void startActivate() {
        ThreadManager.getInstance().runAsync(new Runnable() {
            @Override
            public void run() {
                postUUID();
            }
        });
    }

    /**
     * 网络请求uuid
     */
    private void postUUID() {
        Logger.d(TAG, "postUUID: ");
        CommonManager.getInstance().insertOrReplace(AutoMapConstant.ActivateOrderTAG.UUID_KEY, "");
        if (!ConvertUtils.isEmpty(CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.UUID_KEY))) {
            UUID = CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.UUID_KEY);
            Logger.d(TAG, "UUID静态变量为空，数据库存有UUID = " , UUID);
            mAdasManager.setUUID(UUID, UuidSubStatus.Unknown);
            readyCreateOrder();
            return;
        }

        final UuidRequest uuidRequest = new UuidRequest(API_VERSION, TEST_APP_ID, SYS_VERSION, DEVICES_ID);
        Logger.d(TAG, "uuid req : " , uuidRequest);
        NetQueryManager.getInstance().queryUuid(uuidRequest, new NetQueryManager.INetResultCallBack<UuidResponse>() {
            @Override
            public void onSuccess(final UuidResponse response) {
                if (Logger.openLog) {
                    Logger.d(TAG, response.toString());
                }
                UUID = response.getMVin();
                CommonManager.getInstance().insertOrReplace(AutoMapConstant.ActivateOrderTAG.UUID_KEY, UUID);
                mAdasManager.setUUID(UUID, UuidSubStatus.Unknown);
                readyCreateOrder();
            }

            @Override
            public void onFailed() {
                Logger.d(TAG, "Uuid网络请求失败");
                if (mNetFailedRetryCount.incrementAndGet() < 3) {
                    Logger.d(TAG, "网络失败计数: " , mNetFailedRetryCount.get());
                    mNetRetryExecutor.schedule(new Runnable() {
                        @Override
                        public void run() {
                            postUUID();
                        }
                    }, NET_FAILED_RETRY_DELAY , TimeUnit.SECONDS);
                } else {
                    Logger.d(TAG, "uuid请求网络失败重试次数用完，等待下次点火");
                    if (!mNetRetryExecutor.isShutdown()) {
                        mNetRetryExecutor.shutdown();
                    }
                }
            }
        });

    }

    /**
     * 准备下单
     */
    public void readyCreateOrder() {
        CommonManager.getInstance().insertOrReplace(AutoMapConstant.ActivateOrderTAG.HQ_ORDER_ID, "");
        final String orderId = CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.HQ_ORDER_ID);
        if (!ConvertUtils.isEmpty(orderId)) {
            Logger.d(TAG, "有订单号记录，直接查询订单 : " , orderId);
            getInstance().queryOrderStatus(new NetQueryManager.INetResultCallBack<QueryOrderResponse>() {
                @Override
                public void onSuccess(final QueryOrderResponse statusBean) {
                    Logger.d(TAG, "firstCheckOrderStatus success");
                    if (Logger.openLog) {
                        Logger.d(TAG, statusBean.toString());
                    }
                    handleStatus(statusBean.getMOrderStatus());
                    if (ConvertUtils.equals(statusBean.getMOrderStatus(), "3")) {
                        //首次查询没有订单后下单
                        createCloudOrder();
                    }
                }

                @Override
                public void onFailed() {
                    Logger.d(TAG, "firstCheckOrderStatus failed");
                    if (mNetFailedRetryCount.incrementAndGet() < 3) {
                        Logger.d(TAG, "网络失败计数 : " , mNetFailedRetryCount.get());
                        mNetRetryExecutor.schedule(new Runnable() {
                            @Override
                            public void run() {
                                readyCreateOrder();
                            }
                        }, NET_FAILED_RETRY_DELAY , TimeUnit.SECONDS);
                    } else {
                        Logger.d(TAG, "网络失败重试次数用完，等待下次点火");
                        if (!mNetRetryExecutor.isShutdown()) {
                            mNetRetryExecutor.shutdown();
                        }
                    }
                }
            });
        } else {
            createCloudOrder();
        }

    }

    /**
     * 云对云下单
     */
    public void createCloudOrder() {
        Logger.d(TAG, "createCloudOrder");
        final String uuid = CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.UUID_KEY);
        if (ConvertUtils.isEmpty(UUID) && ConvertUtils.isEmpty(uuid)) {
            Logger.e(TAG, "uuid信息缺失");
            postUUID();
            return;
        }

        if (!ConvertUtils.equals(UUID, uuid) && !ConvertUtils.isEmpty(uuid)) {
            Logger.d(TAG, "UUID constant = " , UUID , " ; UUID in database = " , uuid);
            UUID = uuid;
        }
        Logger.i(TAG, "UUID = " , UUID);
        final CreateOrderRequest createOrderReq = new CreateOrderRequest(API_VERSION, TEST_APP_ID, UUID, HQ);
        Logger.d(TAG, "createOrderReq  : " , createOrderReq);
        NetQueryManager.getInstance().createOrder(createOrderReq, new NetQueryManager.INetResultCallBack<CreateOrderResponse>() {
            @Override
            public void onSuccess(final CreateOrderResponse response) {
                Logger.d(TAG, response.toString());
                ORDER_ID = response.getMCusOrderId();
                CommonManager.getInstance().insertOrReplace(AutoMapConstant.ActivateOrderTAG.HQ_ORDER_ID, response.getMCusOrderId());
                pollOrderStatusWithRetry();
            }

            @Override
            public void onFailed() {
                if (mNetFailedRetryCount.incrementAndGet() < 3) {
                    Logger.d(TAG, "网络失败计数 : " , mNetFailedRetryCount.get());
                    mNetRetryExecutor.schedule(new Runnable() {
                        @Override
                        public void run() {
                            createCloudOrder();
                        }
                    }, NET_FAILED_RETRY_DELAY , TimeUnit.SECONDS);
                } else {
                    Logger.d(TAG, "下单网络失败重试次数用完，等待下次点火");
                    if (!mNetRetryExecutor.isShutdown()) {
                        mNetRetryExecutor.shutdown();
                    }
                }
            }
        });
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
        if (!mNetRetryExecutor.isShutdown()) {
            mNetRetryExecutor.shutdownNow();
        }
        final Runnable task = new Runnable() {
            @Override
            public void run() {
                if (future.isDone()) {
                    return;
                }
                try {
                    final NetQueryManager.INetResultCallBack<QueryOrderResponse> callBack = new NetQueryManager.INetResultCallBack<QueryOrderResponse>() {
                        @Override
                        public void onSuccess(final QueryOrderResponse statusBean) {
                            Logger.d(TAG, "checkOrderStatus success");
                            Logger.d(TAG, statusBean.toString());

                            handleStatus(statusBean.getMOrderStatus());
                            if (ConvertUtils.equals(statusBean.getMOrderStatus(), "2")) {
                                future.complete(true);
                                executor.shutdownNow();
                            } else if (ConvertUtils.equals(statusBean.getMOrderStatus(), "1")) {
                                Logger.d(TAG, "已下单/等待交付");
                                final int currentCount = retryCount.incrementAndGet();
                                Logger.d(TAG, "轮询次数 : " , currentCount);
                                if (currentCount > maxRetries) {
                                    future.complete(false);
                                    executor.shutdownNow();
                                } else {
                                    executor.schedule(taskRef.get(), delays[currentCount], TimeUnit.MINUTES);
                                }
                            } else if (ConvertUtils.equals(statusBean.getMOrderStatus(), "3")) {
                                ++QUERY_ORDER_NUM;
                                Logger.e(TAG, "HQ下单失败" + QUERY_ORDER_NUM + "次");
                                if (QUERY_ORDER_NUM <= 3) {
                                    createCloudOrder();
                                } else {
                                    Logger.e(TAG, "云端返回失败订单结果，重新下单次数用完，等待下次启动车辆");
                                }
                                future.complete(false);
                                executor.shutdownNow();
                            }
                        }
                        @Override
                        public void onFailed() {
                            Logger.d(TAG, "查询订单网络请求失败");
                            final int currentCount = retryCount.incrementAndGet();

                            Logger.d(TAG, "轮询次数 : " , currentCount);
                            if (currentCount > maxRetries) {
                                future.complete(false);
                                executor.shutdownNow();
                                Logger.d(TAG, "最后一次查询网络请求失败，等待下次汽车启动重新激活HQ");
                            } else {
                                executor.schedule(taskRef.get(), delays[currentCount], TimeUnit.MINUTES);
                            }
                        }
                    };
                    getInstance().queryOrderStatus(callBack);
                } catch (NullPointerException | IllegalArgumentException e) {
                    future.completeExceptionally(e);
                    executor.shutdownNow();
                }
            }
        };
        taskRef.set(task);
        executor.schedule(task, delays[0], TimeUnit.MINUTES);

        try {
            // 总超时 = 所有可能延迟之和 + 缓冲时间（例如15分钟）
            final long totalTimeout = Arrays.stream(delays).sum() + 1; // 2+5+5 +1=13分钟
            return future.get(totalTimeout, TimeUnit.MINUTES);
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
     * @param callBack 网络结果回调
     */
    public void queryOrderStatus(final NetQueryManager.INetResultCallBack<QueryOrderResponse> callBack) {
        Logger.d(TAG, "checkOrderStatus");
        final String orderId = CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.HQ_ORDER_ID);

        if (ConvertUtils.isEmpty(ORDER_ID) && ConvertUtils.isEmpty(orderId)) {
            Logger.e(TAG, "订单id信息缺失");
            return;
        }

        if (!ConvertUtils.equals(ORDER_ID, orderId) && !ConvertUtils.isEmpty(orderId)) {
            Logger.d(TAG, "ORDER_ID constant = " , ORDER_ID , " ; orderId in database = " , orderId);
            ORDER_ID = orderId;
        }
        Logger.i(TAG, "ORDER_ID = " , ORDER_ID);
        final QueryOrderRequest queryOrderRequest = new QueryOrderRequest(SYS_VERSION, TEST_APP_ID, UUID, ORDER_ID);
        Logger.d(TAG, "queryOrderRequest  : " , queryOrderRequest);
        NetQueryManager.getInstance().queryOrder(queryOrderRequest, callBack);

    }

    /**
     * 处理查单状态
     *
     * @param status statusBean
     */
    private void handleStatus(final String status) {
        Logger.i(TAG, "status = " , status);
        if (ConvertUtils.equals(status, "1")) {
            mAdasManager.setUUID(UUID, UuidSubStatus.Unknown);
        } else if (ConvertUtils.equals(status, "2")) {
            mAdasManager.setUUID(UUID, UuidSubStatus.Subscribed);
        } else if (ConvertUtils.equals(status, "3")) {
            mAdasManager.setUUID(UUID, UuidSubStatus.Unsubscribed);
        } else {
            Logger.e(TAG, "未知状态");
        }
    }

}
