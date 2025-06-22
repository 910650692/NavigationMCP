package com.sgm.navi.scene.ui.navi;


import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ToastUtils;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.SceneNaviParallelViewBinding;
import com.sgm.navi.scene.impl.navi.SceneNaviParallelImpl;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneBase;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.AppCache;

/**
 * 主辅路、桥上桥下
 * @author sgm
 * @version $Revision.*$
 */
public class SceneNaviParallelView extends NaviSceneBase<SceneNaviParallelViewBinding, SceneNaviParallelImpl> {

    public SceneNaviParallelView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviParallelView(@NonNull final Context context,
                                 @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviParallelView(@NonNull final Context context, @Nullable final AttributeSet attrs,
                                 final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_PARALLEL;
    }

    @Override
    protected SceneNaviParallelViewBinding createViewBinding(final LayoutInflater inflater,
                                                             final ViewGroup viewGroup) {
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
    public void show() {
        super.show();
    }

    @Override
    public boolean onTouchEvent(final MotionEvent event) {
        return true;
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
        ToastUtils.Companion.getInstance().showCustomToastView(
                AppCache.getInstance().getMContext().
                        getText(R.string.navi_switch_to_road_auxiliary));
    }

    /***Toast：已为您切换至主路***/
    public void showToastRoadSideToMain() {
        ToastUtils.Companion.getInstance().showCustomToastView(AppCache.getInstance().getMContext().getText(R.string.navi_switch_to_road_main));
    }

    /***Toast：已为您切换至主高架下***/
    public void showToastBridgeUpToDown() {
        ToastUtils.Companion.getInstance().showCustomToastView(AppCache.getInstance().getMContext().getText(R.string.navi_switch_to_bridge_down));
    }

    /***Toast：已为您切换至主高架上***/
    public void showToastBridgeDownToUp() {
        ToastUtils.Companion.getInstance().showCustomToastView(AppCache.getInstance().getMContext().getText(R.string.navi_switch_to_bridge_up));
    }

    /**
     * @param type 平行路切换类型 0:主辅路切换 1:桥上下切换
     */
    public void naviParallelSwitch(final int type) {
        mScreenViewModel.naviParallelSwitch(type);
    }
}
