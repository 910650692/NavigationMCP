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

    private Paint paint;
    private BitmapShader shader;
    private int radius;
    private float mScale;
    private Matrix matrix;

    public SceneCircleImageView(Context context) {
        super(context);
        init();
    }

    public SceneCircleImageView(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    public SceneCircleImageView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    private void init(){
        paint = new Paint(Paint.ANTI_ALIAS_FLAG);
        matrix = new Matrix();
        paint.setAntiAlias(true);
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);

        int minTarget = Math.min(getMeasuredWidth(), getMeasuredHeight());
        radius = minTarget / 2;
        setMeasuredDimension(minTarget, minTarget);
    }

    @Override
    protected void onDraw(Canvas canvas) {
        Bitmap bitmap = drawable2Bitmap(getDrawable());
        if(bitmap == null){
            bitmap = BitmapFactory.decodeResource(getResources(), R.mipmap.default_user_icon);
        }

        shader = new BitmapShader(bitmap, Shader.TileMode.CLAMP, Shader.TileMode.CLAMP);
        mScale = (radius * 2.0f) / Math.min(bitmap.getWidth(), bitmap.getHeight());

        matrix.setScale(mScale, mScale);
        shader.setLocalMatrix(matrix);

        paint.setShader(shader);
        canvas.drawCircle(radius, radius, radius, paint);
    }

    private Bitmap drawable2Bitmap(Drawable drawable){
        if (drawable instanceof BitmapDrawable bd) {
            return bd.getBitmap();
        }
        int w = drawable.getIntrinsicWidth();
        int h = drawable.getIntrinsicHeight();
        Bitmap bitmap = Bitmap.createBitmap(w, h, Bitmap.Config.ARGB_8888);
        Canvas canvas = new Canvas(bitmap);
        drawable.setBounds(0, 0, w, h);
        drawable.draw(canvas);
        return bitmap;
    }
}