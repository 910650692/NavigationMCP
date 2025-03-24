package com.fy.navi.scene.ui.navi.manager;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.databinding.ViewDataBinding;

import com.android.utils.ConvertUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.concurrent.ScheduledFuture;

import lombok.Getter;
import lombok.Setter;

/**
 * 导航卡片抽象类
 * 定义卡片id、状态及实现倒计时关闭功能
 * @author fy
 * @version $Revision.*$
 * @param <VB>
 * @param <VM>
 */
@Getter
@Setter
public abstract class NaviSceneBase <VB extends ViewDataBinding, VM extends BaseSceneModel> extends BaseSceneView<VB, VM> {
    private static final String TAG = "NaviCardBase";
    public static final int SCENE_STATE_INIT = 0x00;
    public static final int SCENE_STATE_SHOW = 0x01;
    public static final int SCENE_STATE_HIDE = 0x02;
    public static final int SCENE_STATE_CLOSE = 0x03;

    protected static final int CLOSE_COUNTDOWN_8 = 8;
    protected static final int CLOSE_COUNTDOWN_5 = 5;
    protected static final int CLOSE_COUNTDOWN_15 = 15;
    protected static final int CLOSE_COUNTDOWN_60 = 60;
    protected boolean mIsViaListExpand = false;//途径点列表是否展开
    protected int mSceneState = SCENE_STATE_INIT;
    private int mCountdown = 0;
    private ScheduledFuture mScheduledFuture;
    private ISceneCallback mISceneCallback;
    private NaviSceneId mSceneId;

    private INaviSceneEvent mEvent = getNaviSceneEvent();

    public NaviSceneBase(@NonNull final Context context) {
        super(context);
    }

    public NaviSceneBase(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public NaviSceneBase(@NonNull final Context context, @Nullable final AttributeSet attrs,
                         final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public boolean isIsViaListExpand() {
        return mIsViaListExpand;
    }

    public void setIsViaListExpand(boolean mIsViaListExpand) {
        this.mIsViaListExpand = mIsViaListExpand;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mSceneId = getSceneId();
        init();
    }

    protected abstract NaviSceneId getSceneId();

    /**
     * @return INaviSceneEvent
     */
    public abstract INaviSceneEvent getNaviSceneEvent();

    protected abstract void init();

    /**
     * @param sceneCallback sceneCallback
     */
    public abstract void addSceneCallback(ISceneCallback sceneCallback);

    protected void unInit() {
        mISceneCallback = null;
    }

    /**
     * 显示卡片，倒计时开始
     */
    public void show() {
        // 新卡片显示需要重置倒计时，隐藏卡片显示使用选线剩余倒计时
        if (mSceneState != SCENE_STATE_HIDE) {
            resetCountdown();
        }
        if (isStartCountdown()) {
            startCountdown();
        }
        mSceneState = SCENE_STATE_SHOW;
    }

    public boolean isStartCountdown() {
        return true;
    }

    /**
     * 隐藏卡片，但是倒计时还在
     */
    public void hide() {
        // 新卡片如果一开始就是隐藏(有高优卡片显示时新卡创建并隐藏)，显示需要重置倒计时
        if (mSceneState == SCENE_STATE_CLOSE || mSceneState == SCENE_STATE_INIT) {
            resetCountdown();
        }
        cancelCountdown();
        mSceneState = SCENE_STATE_HIDE;
    }

    /**
     * 关闭卡片，不再响应任何事件
     */
    public void close() {
        mCountdown = 0;
        cancelCountdown();
        mSceneState = SCENE_STATE_CLOSE;
    }

    public int getSceneState() {
        return mSceneState;
    }

    protected void resetCountdown() {
        switch (mSceneId) {
//            case CARD_ENERGY_EARLY_WARNING:
//            case CARD_REALTIME_MSG:
//                mCountdown = CLOSE_COUNTDOWN_60;
//                break;
            case NAVI_SCENE_SERVICE_AREA:
            case NAVI_SCENE_VIA_POINT_UNFOLD:
                mCountdown = CLOSE_COUNTDOWN_15;
                break;
            default:
                mCountdown = CLOSE_COUNTDOWN_8;
                break;
        }
    }

    protected void startCountdown() {
        if (mCountdown == 0 || mScheduledFuture != null) {
            return;
        }
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            if (mCountdown == NumberUtils.NUM_0) {
                if (mEvent != null) {
                    if (mSceneId == NaviSceneId.NAVI_SCENE_VIA_POINT_FOLD) {
                        // 只有经过所有途经点才会close，倒计时场景只会hide途经点卡片
//                        mEvent.notifySceneStateChange(INaviSceneEvent.SceneStateChangeType.SceneHideState, mCardId);
                    } else {
//                        mEvent.notifySceneStateChange(INaviSceneEvent.SceneStateChangeType.SceneCloseState, mCardId);
                    }
                }
                cancelCountdown();
            }
            --mCountdown;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }

    protected void cancelCountdown() {
        if (mScheduledFuture != null) {
            if (!ConvertUtils.isEmpty(mScheduledFuture)) {
                ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
                mScheduledFuture = null;
            }
        }
    }
}
