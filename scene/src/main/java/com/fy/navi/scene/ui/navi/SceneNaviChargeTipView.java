package com.fy.navi.scene.ui.navi;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.fy.navi.scene.databinding.SceneNaviChargeTipViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviChargeTipViewImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;

/**
 * @author : QiuYaWei
 * @version $Revision.*$
 * Description: [充电站相关提示展示]
 */
public class SceneNaviChargeTipView extends NaviSceneBase<SceneNaviChargeTipViewBinding, SceneNaviChargeTipViewImpl> {
    private static final String TAG = "SceneNaviChargeTipView";
    private ChargeTipEntity mEntity;

    public SceneNaviChargeTipView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviChargeTipView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviChargeTipView(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_CHARGE_TIP;
    }

    @Override
    protected String getSceneName() {
        return TAG;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    @Override
    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NaviSceneId.NAVI_CHARGE_TIP, this);
        mViewBinding.tvAction.setOnClickListener((view) -> {
            if (mEntity == null) {
                return;
            }
            switch (mEntity.getType()) {
                case SceneNaviChargeBtnType.GO_CHARGING -> {
                    if (mISceneCallback != null) {
                        mISceneCallback.goCharge();
                    }
                }
                case SceneNaviChargeBtnType.SEARCH_NEW_STATION -> {
                    if (mISceneCallback != null) {
                        mISceneCallback.searchNewChargeStation();
                    }
                }
                case SceneNaviChargeBtnType.OPEN_SUPPLY -> {
                    if (mISceneCallback != null) {
                        mISceneCallback.openSupplyPlan();
                    }
                }
                case SceneNaviChargeBtnType.I_KNOW -> {
                }
                default -> {
                    Logger.i(TAG, "此类型不支持，请检查代码！");
                }
            }
            // 隐藏弹窗
            hide();
        });
    }

    @Override
    public void show() {
        super.show();
        Logger.i(TAG, "show!", "callBack is null:" + (mISceneCallback == null));
        mScreenViewModel.initTimer();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_CHARGE_TIP, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        Logger.i(TAG, "hide!", "callBack is null:" + (mISceneCallback == null));
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_CHARGE_TIP, false);
        }
    }

    @Override
    public void close() {
        super.close();
        Logger.i(TAG, "close!", "callBack is null:" + (mISceneCallback == null));
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_CHARGE_TIP, false);
        }
    }

    @Override
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        Logger.i(TAG, "addSceneCallback success!");
        mISceneCallback = sceneCallback;
    }

    @Override
    protected void unInit() {
        super.unInit();
        Logger.i(TAG, "unInit success!");
    }

    @Override
    protected SceneNaviChargeTipViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneNaviChargeTipViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviChargeTipViewImpl initSceneImpl() {
        return new SceneNaviChargeTipViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {

    }

    @Override
    protected void initObserver() {

    }

    /***
     * 更新UI内容
     * @param entity
     */
    public void updateUi(final ChargeTipEntity entity) {
        this.mEntity = entity;
        if (entity == null) {
            return;
        }
        mViewBinding.tvTitle.setText(entity.getTitle());
        mViewBinding.tvAction.setVisibility((TextUtils.isEmpty(entity.getAction()) ? View.GONE : View.VISIBLE));
        mViewBinding.tvAction.setText(entity.getAction());
        mViewBinding.tvDesc.setVisibility(TextUtils.isEmpty(entity.getSubTitle()) ? View.GONE : View.VISIBLE);
        mViewBinding.tvDesc.setText(entity.getSubTitle());
        getNaviSceneEvent().notifySceneStateChange(INaviSceneEvent.SceneStateChangeType.SceneShowState, getSceneId());
        Logger.i(TAG, "updateUi");
    }
}
