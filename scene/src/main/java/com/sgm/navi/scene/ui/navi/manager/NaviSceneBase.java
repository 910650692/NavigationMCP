package com.sgm.navi.scene.ui.navi.manager;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.CallSuper;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.databinding.ViewDataBinding;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.service.define.utils.NumberUtils;

import java.util.concurrent.ScheduledFuture;

import lombok.Getter;
import lombok.Setter;

/**
 * 导航卡片抽象类
 * 定义卡片id、状态及实现倒计时关闭功能
 *
 * @param <VB>
 * @param <VM>
 * @author sgm
 * @version $Revision.*$
 */
@Getter
@Setter
public abstract class NaviSceneBase<VB extends ViewDataBinding, VM extends BaseSceneModel> extends BaseSceneView<VB, VM> {
    private static final String TAG = "NaviCardBase";
    protected static final int SCENE_STATE_INIT = 0x00;
    public static final int SCENE_STATE_SHOW = 0x01;
    protected static final int SCENE_STATE_HIDE = 0x02;
    protected static final int SCENE_STATE_CLOSE = 0x03;

    protected static final int CLOSE_COUNTDOWN_8 = 8;
    protected static final int CLOSE_COUNTDOWN_5 = 5;
    protected static final int CLOSE_COUNTDOWN_15 = 15;
    protected static final int CLOSE_COUNTDOWN_60 = 60;
    protected int mSceneState = SCENE_STATE_INIT;
    private int mCountdown = 0;
    private ScheduledFuture mScheduledFuture;
    protected ISceneCallback mISceneCallback;
    public NaviSceneId mSceneId;
    // 添加一个标签，scene会被服用，用来判断是哪个模块服用的此view
    private int mViewCategory;

    private INaviSceneEvent mEvent = getNaviSceneEvent();

    protected abstract NaviSceneId getSceneId();

    public NaviSceneId getMSceneId() {
        if (mSceneId == null) {
            return getSceneId();
        }
        return mSceneId;
    }

    public void setCategory(int tag) {
        mViewCategory = tag;
    }

    public int getCategory() {
        return mViewCategory;
    }

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

    @Override
    public void onCreate() {
        super.onCreate();
        mSceneId = getSceneId();
        init();
    }

    public String getSceneName() {
        final boolean idIsNull = ConvertUtils.isNull(getSceneId());
//        String sceneName = idIsNull ? "unknownName" : getSceneId().name();
//        Logger.i(TAG, "getSceneName:" + sceneName);
        return idIsNull ? "unknownName" : getSceneId().name();
    }

    /**
     * @return INaviSceneEvent
     */
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    @CallSuper
    protected void init() {
        addNaviScene();
    }

    public void addNaviScene() {
        NaviSceneManager.getInstance().addNaviScene(getSceneId(), this);
    }

    /**
     * @param sceneCallback sceneCallback
     */
    public void addSceneCallback(ISceneCallback sceneCallback) {
        this.mISceneCallback = sceneCallback;
        if (!ConvertUtils.isNull(mScreenViewModel)) {
            mScreenViewModel.addCallBack(sceneCallback);
        }
    }

    /**
     * 显示卡片，倒计时开始
     */
    public void show() {
        Logger.d(TAG, "show:" , getClass().getSimpleName(), "callBack :" , (mISceneCallback != null));
        // 新卡片显示需要重置倒计时，隐藏卡片显示使用选线剩余倒计时
        if (mSceneState != SCENE_STATE_HIDE) {
            resetCountdown();
        }
        if (!ConvertUtils.isNull(mISceneCallback)) {
            mISceneCallback.updateSceneVisible(getSceneId(), true);
            mSceneState = SCENE_STATE_SHOW;
            startCountdown();
        }
    }

    /**
     * 显示卡片，倒计时开始
     */
    public void show(INaviSceneEvent.SceneInfo info) {
        Logger.d(TAG, "show:" , getClass().getSimpleName(), "callBack :" , (mISceneCallback == null));
        // 新卡片显示需要重置倒计时，隐藏卡片显示使用选线剩余倒计时
        if (mSceneState != SCENE_STATE_HIDE) {
            resetCountdown();
        }
        if (!ConvertUtils.isNull(mISceneCallback)) {
            mISceneCallback.updateSceneVisible(getSceneId(), true);
            mSceneState = SCENE_STATE_SHOW;
            startCountdown();
        }
    }

    /***
     * 是否主动开启倒计时
     * @return false 不处理，如果需要处理重写返回true即可
     */
    public boolean isNeedAutoStartTimer() {
        return false;
    }

    /**
     * 隐藏卡片，但是倒计时还在
     */
    public void hide() {
        Logger.d(TAG, "hide:" , getClass().getSimpleName(), "callBack :" , (mISceneCallback == null));
        // 新卡片如果一开始就是隐藏(有高优卡片显示时新卡创建并隐藏)，显示需要重置倒计时
        if (mSceneState == SCENE_STATE_CLOSE || mSceneState == SCENE_STATE_INIT) {
            resetCountdown();
        }
        cancelCountdown();
        if (!ConvertUtils.isNull(mISceneCallback)) {
            mSceneState = SCENE_STATE_HIDE;
            mISceneCallback.updateSceneVisible(getSceneId(), false);
        }
    }

    /**
     * 关闭卡片，不再响应任何事件
     */
    public void close() {
        Logger.d(TAG, "close:" , getClass().getSimpleName(), "callBack :" , (mISceneCallback == null));
        mCountdown = 0;
        cancelCountdown();
        if (!ConvertUtils.isNull(mISceneCallback)) {
            mSceneState = SCENE_STATE_CLOSE;
            mISceneCallback.updateSceneVisible(getSceneId(), false);
            //mEvent.notifySceneReset("close");
        }
    }

    public int getSceneState() {
        return mSceneState;
    }

    public void setSceneState(int state) {
        mSceneState = state;
    }

    public boolean isVisible() {
        return SCENE_STATE_SHOW == mSceneState;
    }

    public boolean isHidden() {
        return SCENE_STATE_HIDE == mSceneState;
    }

    public void resetCountdown() {
        switch (mSceneId) {
            case NAVI_SCENE_SERVICE_AREA:
            case NAVI_SCENE_VIA_POINT_LIST:
                mCountdown = CLOSE_COUNTDOWN_15;
                break;
            default:
                mCountdown = CLOSE_COUNTDOWN_8;
                break;
        }
    }

    public void startCountdown() {
        if (mCountdown == 0 || !isNeedAutoStartTimer()) {
            return;
        }
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            if (mCountdown == NumberUtils.NUM_0) {
                notifySceneStateChange(false, true);
                cancelCountdown();
            }
            --mCountdown;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }

    public void cancelCountdown() {
        if (!ConvertUtils.isNull(mScheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mScheduledFuture = null;
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        cancelCountdown();
        mISceneCallback = null;
    }

    /***
     * 根据需要复写
     * @param isVisible
     */
    public void notifySceneStateChange(final boolean isVisible, boolean isReset) {
        final boolean isCanDo = isVisible != isVisible();
        Logger.i(TAG, "clsName:" , getClass().getSimpleName(), "notifySceneStateChange success", "isCanDo:" , isCanDo , ",isVisible：" , isVisible);
        if (isCanDo) {
            getNaviSceneEvent().notifySceneStateChangeReset((isVisible ?
                            INaviSceneEvent.SceneStateChangeType.SceneShowState :
                    INaviSceneEvent.SceneStateChangeType.SceneCloseState), getSceneId(), isReset);
        }
    }
}
