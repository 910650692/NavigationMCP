package com.sgm.navi.scene.dialog;

import android.animation.ValueAnimator;
import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Build;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;
import android.view.animation.LinearInterpolator;
import android.widget.TextView;

import com.android.utils.ResourceUtils;
import com.sgm.navi.scene.R;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;


/**
 * @author pengbai
 * @version \$Revision1.0\$
 * @Description: 搜索加载框
 * @CreateDate: $ $
 */
public class RouteLoadingDialog extends Dialog {
    private ValueAnimator mAnimator;
    private float mAngelTemp = 0;
    private TextView mTvMessage;
    protected IBaseDialogClickListener mDialogClickListener;

    public RouteLoadingDialog(final Context context) {
        super(context);
    }

    public void setDialogClickListener(final IBaseDialogClickListener dialogClickListener) {
        this.mDialogClickListener = dialogClickListener;
    }

    /**
     * 显示离线算路文言
     */
    public void showOfflineRouting() {
        if (null != mTvMessage)
            mTvMessage.setText(ResourceUtils.Companion.getInstance().getText(R.string.route_offline_loading));
    }

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.layout_route_loading);
        mTvMessage = findViewById(R.id.tv_message);
        initLoadAnim(findViewById(R.id.iv_loading));
        findViewById(R.id.iv_close).setOnClickListener(v -> dismiss());
        setOnDismissListener(dialog -> {
            if (null != mDialogClickListener) mDialogClickListener.onCancelClick();
        });
    }

    @Override
    protected void onStop() {
        super.onStop();
        stopAnimator();
    }

    @Override
    public void show() {
        super.show();
        setLayoutParameterFullScreen();
        if (mAnimator != null && !mAnimator.isRunning()) {
            mAnimator.start();
        }
    }

    /**
     * 初始化加载动画
     *
     * @param sivLoading 加载动画视图
     */
    private void initLoadAnim(final View sivLoading) {
        if (sivLoading == null) {
            return;
        }
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
     *
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
    }

    /**
     * 设置 Dialog 的宽度和高度，使其全屏显示
     */
    private void setLayoutParameterFullScreen() {
        final Window window = getWindow();
        if (window != null) {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
                window.setBackgroundDrawable(new ColorDrawable(Color.TRANSPARENT));
                // 清除可能存在的透明状态栏和导航栏标志
                window.clearFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS |
                        WindowManager.LayoutParams.FLAG_TRANSLUCENT_NAVIGATION);
                // 让内容布局延伸到状态栏和导航栏之下
                window.getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN |
                        View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION |
                        View.SYSTEM_UI_FLAG_LAYOUT_STABLE);
                // 允许设置状态栏和导航栏的背景颜色
                window.addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);
                // 可设置状态栏和导航栏的颜色，这里设置为透明
                window.setStatusBarColor(0x00000000);
                window.setNavigationBarColor(0x00000000);
            } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                // 对于 Android 4.4 到 5.0 的系统，使用透明状态栏和导航栏
                window.addFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
                window.addFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_NAVIGATION);
            }
            WindowManager.LayoutParams params = window.getAttributes();
            params.dimAmount = 0.0f;
            window.setAttributes(params);
            // 将 Dialog 的宽度和高度设置为全屏
            window.setLayout(WindowManager.LayoutParams.MATCH_PARENT,
                    WindowManager.LayoutParams.MATCH_PARENT);
        }
    }
}
