package com.fy.navi.scene.ui.search;

import android.animation.ValueAnimator;
import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.view.animation.LinearInterpolator;

import com.fy.navi.scene.databinding.LayoutSearchLoadingBinding;
import com.fy.navi.ui.dialog.BaseFullScreenDialog;

/**
 * @Author: pengbai
 * @Description: 搜索加载框
 * @CreateDate: $ $
 */
public class RouteSearchLoadingDialog extends BaseFullScreenDialog<LayoutSearchLoadingBinding> {
    private ValueAnimator animator;
    private float angelTemp = 0;

    public RouteSearchLoadingDialog(Context context) {
        super(context);
    }

    @Override
    protected LayoutSearchLoadingBinding initLayout() {
        return LayoutSearchLoadingBinding.inflate(getLayoutInflater());
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        initLoadAnim(mViewBinding.ivLoading);
        mViewBinding.ivClose.setOnClickListener(v -> {
            hide();
        });
    }

    private void initLoadAnim(View sivLoading) {
        // 如果动画已存在并正在运行，则取消并清理
        if (animator != null) {
            if (animator.isRunning()) {
                animator.cancel();
            }
            animator = null;
        }

        // 创建属性动画，从 0 到 360 度循环旋转
        animator = ValueAnimator.ofFloat(0f, 360f);
        animator.setDuration(2000); // 动画持续时间
        animator.setRepeatCount(ValueAnimator.INFINITE); // 无限重复
        animator.setInterpolator(new LinearInterpolator()); // 线性插值器
        // 添加动画更新监听器
        animator.addUpdateListener(animation -> {
            float angle = (float) animation.getAnimatedValue();
            if (shouldSkipUpdate(angle)) {
                return;
            }
            sivLoading.setRotation(angle);
        });
    }

    // 用于控制角度变化频率的辅助方法
    private boolean shouldSkipUpdate(float angle) {
        float changeAngle = angle - angelTemp;
        float angle_step = 10;
        if (changeAngle > 0f && changeAngle <= angle_step) {
            return true; // 跳过更新，避免高频调用浪费资源
        }
        angelTemp = angle; // 更新临时角度值
        return false;
    }

    private void stopAnimator() {
        if (animator == null) {
            return;
        }
        if (animator.isRunning()) {
            animator.cancel();
        }
        animator = null;
    }

    @Override
    public void show() {
        super.show();
        if (animator != null && !animator.isRunning()) {
            animator.start();
        }
    }

    @Override
    public void hide() {
        super.hide();
        stopAnimator();
    }
}
