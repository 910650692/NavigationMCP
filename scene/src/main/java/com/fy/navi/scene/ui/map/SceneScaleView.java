package com.fy.navi.scene.ui.map;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.databinding.SceneScaleBinding;
import com.fy.navi.scene.impl.map.SceneScaleImpl;

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
    public void updateOnMapLevelChanged(String scale) {
        Logger.d("updateOnMapLevelChanged", "scale:" + scale);
        if (ConvertUtils.isNull(mViewBinding) || ConvertUtils.isNull(mViewBinding.screenScaleSize)) {
            Logger.e("updateOnMapLevelChanged", "binding is null:" + (mViewBinding == null), "view is null:" + (mViewBinding.screenScaleSize == null));
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            mViewBinding.screenScaleSize.setText(scale);

            if (scale.equals("1000公里")) {
                mViewBinding.screenScaleReduce.setAlpha(0.5f);
            } else if (scale.equals("10米")) {
                mViewBinding.screenScaleAdd.setAlpha(0.5f);
            } else {
                mViewBinding.screenScaleAdd.setAlpha(1.0f);
                mViewBinding.screenScaleReduce.setAlpha(1.0f);
            }
        });
    }
}
