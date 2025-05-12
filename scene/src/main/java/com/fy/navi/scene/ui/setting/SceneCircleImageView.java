package com.fy.navi.scene.ui.setting;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.BitmapShader;
import android.graphics.Canvas;
import android.graphics.Matrix;
import android.graphics.Paint;
import android.graphics.Shader;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;

import androidx.annotation.Nullable;

import com.fy.navi.scene.R;
import com.fy.navi.ui.view.SkinImageView;

public class SceneCircleImageView extends SkinImageView {

    private Paint mPaint;
    private BitmapShader mShader;
    private int mRadius;
    private float mScale;
    private Matrix mMatrix;

    public SceneCircleImageView(final Context context) {
        super(context);
        init();
    }

    public SceneCircleImageView(final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    public SceneCircleImageView(final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    /**
     * 初始化
     */
    private void init(){
        mPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
        mMatrix = new Matrix();
        mPaint.setAntiAlias(true);
    }

    @Override
    protected void onMeasure(final int widthMeasureSpec, final int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);

        final int minTarget = Math.min(getMeasuredWidth(), getMeasuredHeight());
        mRadius = minTarget / 2;
        setMeasuredDimension(minTarget, minTarget);
    }

    @Override
    protected void onDraw(final Canvas canvas) {
        Bitmap bitmap = drawable2Bitmap(getDrawable());
        if(bitmap == null){
            bitmap = BitmapFactory.decodeResource(getResources(), R.drawable.img_default_user_icon);
        }

        mShader = new BitmapShader(bitmap, Shader.TileMode.CLAMP, Shader.TileMode.CLAMP);
        mScale = (mRadius * 2.0f) / Math.min(bitmap.getWidth(), bitmap.getHeight());

        mMatrix.setScale(mScale, mScale);
        mShader.setLocalMatrix(mMatrix);

        mPaint.setShader(mShader);
        canvas.drawCircle(mRadius, mRadius, mRadius, mPaint);
    }

    /**
     * Drawable转Bitmap
     * @param drawable Drawable
     * @return Bitmap
     */
    private Bitmap drawable2Bitmap(final Drawable drawable){
        if (drawable instanceof BitmapDrawable bd) {
            return bd.getBitmap();
        }
        final int w = drawable.getIntrinsicWidth();
        final int h = drawable.getIntrinsicHeight();
        final Bitmap bitmap = Bitmap.createBitmap(w, h, Bitmap.Config.ARGB_8888);
        final Canvas canvas = new Canvas(bitmap);
        drawable.setBounds(0, 0, w, h);
        drawable.draw(canvas);
        return bitmap;
    }
}