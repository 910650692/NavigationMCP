package com.sgm.navi.scene.ui.navi.hangingcard;

import android.content.Context;
import android.view.View;

import androidx.annotation.CallSuper;
import androidx.annotation.NonNull;
import androidx.databinding.ViewDataBinding;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneManager;
import com.sgm.navi.service.define.navi.HandCardType;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.ui.view.SkinConstraintLayout;
import com.sgm.navi.ui.view.SwipeDeleteLayout;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledFuture;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/18
 * Description: [悬挂卡布局基类]
 */
public abstract class CardView<T extends ViewDataBinding> extends SkinConstraintLayout implements IRouteResultObserver, SwipeDeleteLayout.OnSwipeActionListener {
    protected final String TAG = getClass().getSimpleName();
    private final MapType mapType = MapType.MAIN_SCREEN_MAIN_MAP;
    protected T mBinding;
    protected HandCardType mType;
    protected final long INTERVAL_TIME = 5; // 单位：秒
    private final long DELAY_TIME = 5;// 单位：秒
    protected OnCardChangeListener mListener;
    private final RoutePackage mRoutePackage;
    private final SearchPackage mSearchPackage;
    // 切换路线，需要算路，这个是算路请求ID
    private long mChangeDestinationId;
    protected ArrayList<PoiInfoEntity> mList = new ArrayList<>();
    protected ScheduledFuture scheduledFuture;
    protected PoiInfoEntity mPoiInfo;

    public CardView(@NonNull Context context, final OnCardChangeListener listener, final List<PoiInfoEntity> list, HandCardType type) {
        super(context);
        Logger.i(TAG, "view create success", "type:" , type.name());
        mRoutePackage = RoutePackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        this.mListener = listener;
        this.mList.clear();
        this.mList.addAll(list);
        this.mType = type;
        mBinding = initViewBinding(context);
        updateUi(mList);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        Logger.i(TAG, "onAttachedToWindow");
        initListener();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        Logger.i(TAG, "onDetachedFromWindow");
        closeTimer();
        mRoutePackage.unRegisterRouteObserver(TAG);
        if (!ConvertUtils.isNull(getSwipeView())) {
            getSwipeView().removeSwipeActionListener();
        }
    }

    @CallSuper
    public void initListener() {
        mRoutePackage.registerRouteObserver(TAG, this);
        if (!ConvertUtils.isNull(getSwipeView())) {
            getSwipeView().setOnSwipeActionListener(this);
        }
        // 立即导航-切换目的地
        if (!ConvertUtils.isNull(getNaviView()) && !ConvertUtils.isEmpty(mList)) {
            getNaviView().setOnClickListener(v -> {
                changeDestination(mList.get(0));
            });
        }
    }

    @Override
    public void onRouteSuccess(String successMsg) {
        IRouteResultObserver.super.onRouteSuccess(successMsg);
        // 改变目的地算路成功
        final Map<MapType, Long> requestIds = mRoutePackage.getRequestIds();
        if (!ConvertUtils.isEmpty(requestIds) && requestIds.containsValue(mChangeDestinationId) && !ConvertUtils.isNull(mListener)) {
            Logger.i(TAG, "onChangeDestinationSuccess");
            mListener.onChangeDestinationSuccess(mType);
        }
    }

    @Override
    public void onDelete(SwipeDeleteLayout layout) {
        Logger.i(TAG, "onDelete:" , mType.name());
        resetTimer();
        if (!ConvertUtils.isNull(mListener)) {
            mListener.onTimerFinished(mType);
        }
    }

    @Override
    public void onStateChanged(boolean onDragging) {
        if (onDragging) {
            pauseTimer();
        } else {
            resumeTimer();
        }
    }

    /***
     * 开启定时器
     */
    public void startTimer() {
        startSchedule(DELAY_TIME);
    }

    /***
     * 暂停定时器
     */
    public void pauseTimer() {
        stopSchedule();
    }

    /****
     * 恢复定时器
     */
    public void resumeTimer() {
        startSchedule(DELAY_TIME);
    }

    /***
     * 关闭定时器
     */
    public void closeTimer() {
        stopSchedule();
    }

    /***
     * 重置定时器
     */
    public void resetTimer() {
        stopSchedule();
    }

    abstract T initViewBinding(Context context);

    /***
     * 切换目的地
     * @param poiInfo 目的地信息
     */
    public void changeDestination(final PoiInfoEntity poiInfo) {
        ThreadManager.getInstance().execute(() -> {
            if(Logger.openLog) {
                Logger.d(TAG, "changeDestination poiInfo:", poiInfo == null ? "null" : poiInfo.getName(),
                        " mapType:", mapType);
            }
            mChangeDestinationId = mRoutePackage.requestChangeEndWithoutVia(mapType, poiInfo);
        });
    }

    /***
     * 展示列表详情
     */
    public void showDetail(final HandCardType type) {
        NaviSceneHandingCardDetail naviSceneBase = (NaviSceneHandingCardDetail) NaviSceneManager.getInstance().getSceneById(NaviSceneId.NAVI_SUSPEND_CARD_DETAIL);
        if (!ConvertUtils.isNull(naviSceneBase)) {
            naviSceneBase.updateUi(mList, type);
        }
    }

    /***
     * 获取侧滑删除控件，需要子类自己实现
     * @return
     */
    public SwipeDeleteLayout getSwipeView() {
        return null;
    }

    /***
     * 获取立即导航的View
     * @return
     */
    public View getNaviView() {
        return null;
    }

    /***
     * 更新UI
     * @param dataList
     */
    abstract void updateUi(List<PoiInfoEntity> dataList);

    /**
     * 是否展开状态
     * @param isExpand
     */
    public void setExpandState(boolean isExpand){}

    private void startSchedule(final long delayTime) {
        try {
            scheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(this::updateDistanceSchedule, delayTime, INTERVAL_TIME);
        } catch (Exception e) {
            Logger.e(TAG, "startSchedule failed:" + e.getMessage());
        }
    }

    private void stopSchedule() {
        try {
            if (!ConvertUtils.isNull(scheduledFuture) && !scheduledFuture.isDone()) {
                final boolean cancelResult = scheduledFuture.cancel(true);
                Logger.i(TAG, "stopSchedule:" , cancelResult);
            } else {
                Logger.i(TAG, "stopSchedule failed, scheduledFuture is null or had completed");
            }
        } catch (Exception e) {
            Logger.e(TAG, "stopSchedule failed:" + e.getMessage());
        }
    }

    private void updateDistanceSchedule() {
        if (ConvertUtils.isNull(mPoiInfo)) {
            Logger.i(TAG, "mPoiInfo is null");
            return;
        }
        GeoPoint geoPoint = mPoiInfo.getPoint();
        if (geoPoint == null) {
            Logger.i(TAG, "geoPoint is null");
            return;
        }
        String distance = mSearchPackage.calcStraightDistance(geoPoint);
        ThreadManager.getInstance().postUi(() -> {
            updateDistance(distance);
        });
    }

    protected void updateDistance(String distance) {
    }
}
