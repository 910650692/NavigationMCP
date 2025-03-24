package com.fy.navi.scene.ui.search;

import android.animation.ValueAnimator;
import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.view.animation.LinearInterpolator;

import com.fy.navi.scene.databinding.LayoutSearchLoadingBinding;
import com.fy.navi.ui.dialog.BaseFullScreenDialog;

/**
 * @author pengbai
 * @version \$Revision1.0\$
 * @Description: 搜索加载框
 * @CreateDate: $ $
 */
public class RouteSearchLoadingDialog extends BaseFullScreenDialog<LayoutSearchLoadingBinding> {
    private ValueAnimator mAnimator;
    private float mAngelTemp = 0;

    public RouteSearchLoadingDialog(final Context context) {
        super(context);
    }

    @Override
    protected LayoutSearchLoadingBinding initLayout() {
        return LayoutSearchLoadingBinding.inflate(getLayoutInflater());
    }

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        initLoadAnim(mViewBinding.ivLoading);
        mViewBinding.ivClose.setOnClickListener(v -> {
            hide();
        });
    }

    /**
     * 初始化加载动画
     * @param sivLoading 加载动画视图
     */
    private void initLoadAnim(final View sivLoading) {
        // 如果动画已存在并正在运行，则取消并清理
        if (mAnimator != null) {
            if (mAnimator.isRunning()) {
                mAnimator.cancel();
            }
            mAnimator = null;
        }

        // 创建属性动画，从 0 到 360 度循环旋转
        mAnimator = ValueAnimator.ofFloat(0f, 360f);
        mAnimator.setDuration(2000); // 动画持续时间
        mAnimator.setRepeatCount(ValueAnimator.INFINITE); // 无限重复
        mAnimator.setInterpolator(new LinearInterpolator()); // 线性插值器
        // 添加动画更新监听器
        mAnimator.addUpdateListener(animation -> {
            final float angle = (float) animation.getAnimatedValue();
            if (shouldSkipUpdate(angle)) {
                return;
            }
            sivLoading.setRotation(angle);
        });
    }

    /**
     * 用于控制角度变化频率的辅助方法
     * @param angle 当前角度
     * @return 是否跳过更新
     */
    private boolean shouldSkipUpdate(final float angle) {
        final float changeAngle = angle - mAngelTemp;
        final float angleStep = 10;
        if (changeAngle > 0f && changeAngle <= angleStep) {
            return true; // 跳过更新，避免高频调用浪费资源
        }
        mAngelTemp = angle; // 更新临时角度值
        return false;
    }

    /**
     * 停止动画
     */
    private void stopAnimator() {
        if (mAnimator == null) {
            return;
        }
        if (mAnimator.isRunning()) {
            mAnimator.cancel();
        }
        mAnimator = null;
    }

    @Override
    public void show() {
        super.show();
        if (mAnimator != null && !mAnimator.isRunning()) {
            mAnimator.start();
        }
    }

    @Override
    public void hide() {
        super.hide();
        stopAnimator();
    }
}
