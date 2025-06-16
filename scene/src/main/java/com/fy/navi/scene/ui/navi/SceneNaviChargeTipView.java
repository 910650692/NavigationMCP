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
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.databinding.SceneNaviChargeTipViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviChargeTipViewImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;

/**
 * @author : QiuYaWei
 * @version $Revision.*$
 * Description: [充电站相关提示展示]
 */
public class SceneNaviChargeTipView extends NaviSceneBase<SceneNaviChargeTipViewBinding, SceneNaviChargeTipViewImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_CHARGE_TIP;
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
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_CHARGE_TIP;
    }

    @Override
    protected void init() {
        super.init();
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
        Logger.i(TAG, "show!", "callBack is null:", (mISceneCallback == null));
        mScreenViewModel.initTimer();
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
        Logger.i(TAG, "updateUi");
        this.mEntity = entity;
        if (entity == null) {
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            mViewBinding.tvTitle.setText(entity.getTitle());
            mViewBinding.tvAction.setVisibility((TextUtils.isEmpty(entity.getAction()) ? View.GONE : View.VISIBLE));
            mViewBinding.tvAction.setText(entity.getAction());
            mViewBinding.tvDesc.setVisibility(TextUtils.isEmpty(entity.getSubTitle()) ? View.GONE : View.VISIBLE);
            mViewBinding.tvDesc.setText(entity.getSubTitle());
            getNaviSceneEvent().notifySceneStateChange(INaviSceneEvent.SceneStateChangeType.SceneShowState, getSceneId());
        });
    }
}
