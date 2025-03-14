package com.fy.navi.scene.ui.navi;

import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_VIA_DETAIL_INFO;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SceneNaviViaInfoViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviViaInfoImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.NaviEtaInfo;

/***显示途径点信息、达到状态***/
public class SceneNaviViaInfoView extends NaviSceneBase<SceneNaviViaInfoViewBinding, SceneNaviViaInfoImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;

    public static final int ONLY_NAME = 0;
    public static final int WITH_ARRIVED_TAG = 1;
    public static final int WITH_COUNT_TAG = 2;

    public SceneNaviViaInfoView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviViaInfoView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviViaInfoView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NAVI_SCENE_VIA_DETAIL_INFO;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NAVI_SCENE_VIA_DETAIL_INFO, this);
    }

    @Override
    public void show() {
        super.show();
        Logger.i(TAG, "show() called");
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_VIA_DETAIL_INFO, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_VIA_DETAIL_INFO, false);
        }
    }

    @Override
    public void close() {
        super.close();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_VIA_DETAIL_INFO, false);
        }
    }

    @Override
    protected SceneNaviViaInfoViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneNaviViaInfoViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviViaInfoImpl initSceneImpl() {
        return new SceneNaviViaInfoImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviViaInfo(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    public void onNaviInfo(NaviEtaInfo naviEtaInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviInfo(naviEtaInfo);
        }
    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
        if (mScreenViewModel != null) {
            mScreenViewModel.addSceneCallback(sceneCallback);
        }
    }

    /**
     * 更新途经点到达状态
     * @param str 途经点名称
     * @param listSize 0
     */
    public void onArriveVia(String str, int listSize) {
        Logger.i(TAG, "onArriveVia: " + str);
        mViewBinding.stvWay.setText(str);
        mViewBinding.stvWayArrive.setVisibility(VISIBLE);
        mViewBinding.stvViaCount.setVisibility(INVISIBLE);
        modifyStvWayWidth(WITH_ARRIVED_TAG);
    }

    /**
     * 更新页面途经点显示信息
     * @param viaName 途经点名称
     * @param totalSize 途经点总数量
     */
    public void updateViaInfo(String viaName, int totalSize) {
        Logger.i(TAG, "updateViaInfo: " + viaName);
        mViewBinding.stvWayArrive.setVisibility(INVISIBLE);
        mViewBinding.stvWay.setText(viaName);
        if (totalSize > 1) {
            mViewBinding.stvViaCount.setVisibility(VISIBLE);
            mViewBinding.stvViaCount.setText(String.format(getResources().
                    getString(R.string.navi_via_count), totalSize));
            modifyStvWayWidth(WITH_COUNT_TAG);
        } else {
            mViewBinding.stvViaCount.setVisibility(INVISIBLE);
            modifyStvWayWidth(ONLY_NAME);
        }
    }

    /**
     * 为了更好的显示效果，这里动态修改途经点到达的显示范围
     * 显示数量信息的时候最大宽度显示为242
     * 显示已到达的时候显示为450
     * 其他情况显示为534
     * @param type 显示类型
     */
    private void modifyStvWayWidth(int type) {
        Logger.i(TAG, "modifyStvWayWidth: " + type);
        LayoutParams params = (LayoutParams)mViewBinding.
                stvWay.getLayoutParams();
        params.matchConstraintMaxWidth = switch (type) {
            case WITH_ARRIVED_TAG ->
                    (int) getResources().getDimension(com.fy.navi.ui.R.dimen.dp_450);
            case WITH_COUNT_TAG -> (int) getResources().getDimension(com.fy.navi.ui.R.dimen.dp_242);
            default -> (int) getResources().getDimension(com.fy.navi.ui.R.dimen.dp_534);
        };
        mViewBinding.stvWay.setLayoutParams(params);
        invalidate();
    }


    public void onUpdateViaPass(long viaIndex) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onUpdateViaPass(viaIndex);
        }
    }

    public void startNavigation() {
        mScreenViewModel.startNavigation();
    }
}
