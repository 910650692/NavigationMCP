package com.sgm.navi.scene.ui.map;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.databinding.SceneScaleBinding;
import com.sgm.navi.scene.impl.map.SceneScaleImpl;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneScaleView extends BaseSceneView<SceneScaleBinding, SceneScaleImpl> {

    public SceneScaleView(@NonNull Context context) {
        super(context);
    }

    public SceneScaleView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneScaleView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneScaleBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneScaleBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneScaleImpl initSceneImpl() {
        return new SceneScaleImpl(this);
    }


    @Override
    protected void setInitVariableId() {
        mViewBinding.setScene(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    /**
     * 更新当前比例尺
     * @param scale current scale
     */
    public void updateOnMapLevelChanged(String scale, int scaleLineVaule) {
        Logger.d("updateOnMapLevelChanged", "scale:" , scale);
        ThreadManager.getInstance().postUi(() -> {
            if (ConvertUtils.isNull(mViewBinding) || ConvertUtils.isNull(mViewBinding.screenScaleSize)) {
                Logger.e("updateOnMapLevelChanged", "binding is null:" + (mViewBinding == null));
                return;
            }
            mViewBinding.screenScaleSize.setText(scale);
            ViewGroup.LayoutParams layoutParams = mViewBinding.screenBgSampleScale.getLayoutParams();
            layoutParams.width = scaleLineVaule;
            mViewBinding.screenBgSampleScale.setLayoutParams(layoutParams);

            if (scale.equals("1000公里")) {
                mViewBinding.screenScaleReduceImg.setAlpha(0.5f);
                mViewBinding.screenScaleReduce.setEnabled(false);
                mViewBinding.screenScaleAddImg.setAlpha(1.0f);
                mViewBinding.screenScaleAdd.setEnabled(true);
            } else if (scale.equals("5米")) {
                mViewBinding.screenScaleAddImg.setAlpha(0.5f);
                mViewBinding.screenScaleAdd.setEnabled(false);
                mViewBinding.screenScaleReduceImg.setAlpha(1.0f);
                mViewBinding.screenScaleReduce.setEnabled(true);
            } else {
                mViewBinding.screenScaleAddImg.setAlpha(1.0f);
                mViewBinding.screenScaleAdd.setEnabled(true);
                mViewBinding.screenScaleReduceImg.setAlpha(1.0f);
                mViewBinding.screenScaleReduce.setEnabled(true);
            }
        });
    }
}
