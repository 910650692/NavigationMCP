package com.fy.navi.scene.ui.navi;

import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_PARALLEL;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ToastUtils;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SceneNaviParallelViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviParallelImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;

/***主辅路、桥上桥下***/
public class SceneNaviParallelView extends NaviSceneBase<SceneNaviParallelViewBinding, SceneNaviParallelImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;

    public SceneNaviParallelView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviParallelView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviParallelView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NAVI_SCENE_PARALLEL;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NAVI_SCENE_PARALLEL, this);
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_PARALLEL, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_PARALLEL, true);
        }
    }

    @Override
    protected SceneNaviParallelViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneNaviParallelViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviParallelImpl initSceneImpl() {
        return new SceneNaviParallelImpl(this);
    }


    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviParallel(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        return true;
    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {
        if (mScreenViewModel != null) {
            mScreenViewModel.addSceneCallback(sceneCallback);
        }
    }

    /***展示切换到主路的状态***/
    public void sceneRoadMain() {
        mViewBinding.stvMainAuxiliary.setText(R.string.navi_road_main);
        mViewBinding.sivMainAuxiliary.setBackgroundResource(R.drawable.img_in_the_main_black_58);
    }

    /***展示切换到辅路的状态***/
    public void sceneRoadSide() {
        mViewBinding.stvMainAuxiliary.setText(R.string.navi_road_auxiliary);
        mViewBinding.sivMainAuxiliary.setBackgroundResource(R.drawable.img_in_the_side_black_58);
    }

    /***展示切换到桥上的状态***/
    public void sceneBridgeUp() {
        mViewBinding.stvBridgeUpDown.setText(R.string.navi_bridge_up);
        mViewBinding.sivBridgeUpDown.setBackgroundResource(R.drawable.img_on_the_bridge_black_58);
    }

    /***展示切换到桥下的状态***/
    public void sceneBridgeDown() {
        mViewBinding.stvBridgeUpDown.setText(R.string.navi_bridge_down);
        mViewBinding.sivBridgeUpDown.setBackgroundResource(R.drawable.img_under_the_bridge_black_58);
    }

    /***Toast：已为您切换至辅路***/
    public void showToastRoadMainToSide() {
        ToastUtils.Companion.getInstance().showCustomToastView(AppContext.mContext.getText(R.string.navi_switch_to_road_auxiliary));
    }

    /***Toast：已为您切换至主路***/
    public void showToastRoadSideToMain() {
        ToastUtils.Companion.getInstance().showCustomToastView(AppContext.mContext.getText(R.string.navi_switch_to_road_main));
    }

    /***Toast：已为您切换至主高架下***/
    public void showToastBridgeUpToDown() {
        ToastUtils.Companion.getInstance().showCustomToastView(AppContext.mContext.getText(R.string.navi_switch_to_bridge_down));
    }

    /***Toast：已为您切换至主高架上***/
    public void showToastBridgeDownToUp() {
        ToastUtils.Companion.getInstance().showCustomToastView(AppContext.mContext.getText(R.string.navi_switch_to_bridge_up));
    }
}
