
package com.fy.navi.scene.ui.adapter;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;

import android.annotation.SuppressLint;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.navi.INaviViaItemClickListener;
import com.fy.navi.scene.databinding.SceneNaviViaListItemBinding;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.ui.view.SkinImageView;
import com.fy.navi.ui.view.SkinTextView;
import com.fy.navi.ui.view.SwipeMenuLayout;

import java.util.ArrayList;
import java.util.List;

public class NaviViaListAdapter extends RecyclerView.Adapter<NaviViaListAdapter.ResultHolder> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private final List<NaviViaEntity> mList;
    private INaviViaItemClickListener onItemClickListener;
    private int powerType;

    public List<NaviViaEntity> getData() {
        return mList;
    }

    public void setOnItemClickListener(INaviViaItemClickListener listener) {
        onItemClickListener = listener;
    }

    public NaviViaListAdapter() {
        this.mList = new ArrayList<>();
        powerType = OpenApiHelper.powerType();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void notifyList(List<NaviViaEntity> list) {
        Logger.d(TAG, "NaviAddViaAdapter notifyList " + list);
        if (ConvertUtils.isEmpty(list)) {
            return;
        }
        mList.clear();
        mList.addAll(list);
        notifyDataSetChanged();
    }

    public void removeData(NaviViaEntity entity) {
        int pos = mList.indexOf(entity);
        if (pos < 0) return;
        this.mList.remove(entity);
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public ResultHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        SceneNaviViaListItemBinding itemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.scene_navi_via_list_item, parent, false);
        return new ResultHolder(itemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull ResultHolder holder, int position) {
        Logger.d(TAG, "NaviAddViaAdapter onBindViewHolder " + position);
        // 非电动车不显示到达电量显示
        if (powerType != 1) {
            holder.itemBinding.stvAddViaElec.setVisibility(GONE);
            holder.itemBinding.sivAddViaElec.setVisibility(GONE);
        } else {
            holder.itemBinding.stvAddViaElec.setVisibility(VISIBLE);
            holder.itemBinding.sivAddViaElec.setVisibility(VISIBLE);
            setChargeUi(mList.get(position).getArriveBatteryLeft(),
                    holder.itemBinding.sivAddViaElec, holder.itemBinding.stvAddViaElec);
        }
        NaviViaEntity naviViaEntity = mList.get(position);
        String name = TextUtils.isEmpty(naviViaEntity.getName()) ?
                holder.itemBinding.getRoot().getContext().getString(R.string.navi_unknown_address) : mList.get(position).getName();
        holder.itemBinding.stvAddViaName.setText(name);
//        holder.itemBinding.setViaBean(naviViaEntity);
        String distance = naviViaEntity.getDistance();
        String arriveTime = naviViaEntity.getArriveTime();
        String arriveDay = naviViaEntity.getArriveDay();
        String none = holder.itemBinding.getRoot().getContext().getString(R.string.navi_none);
        holder.itemBinding.stvAddViaDistance.setText(distance == null ? none : distance);
        holder.itemBinding.stvArriveTime.setText(arriveTime == null ? none : arriveTime);
        holder.itemBinding.stvArrivalDay.setText(arriveDay);
        if (position == mList.size() - 1) {
            holder.itemBinding.stvAddViaIcon.setBackgroundResource(R.drawable.img_navi_via_item_btn_end);
            holder.itemBinding.stvAddViaIcon.setText(R.string.navi_via_item_end);
            holder.itemBinding.llActionContainer.setVisibility(GONE);
            //holder.itemBinding.groupEta.setVisibility(GONE);
        } else {
            holder.itemBinding.stvAddViaIcon.setBackgroundResource(R.drawable.img_navi_via_item_btn_pass);
            holder.itemBinding.stvAddViaIcon.setText(R.string.navi_via_item_pass);
            holder.itemBinding.llActionContainer.setVisibility(VISIBLE);
            //holder.itemBinding.groupEta.setVisibility(VISIBLE);
        }
        holder.itemBinding.clContent.setOnClickListener(v -> {
            if (onItemClickListener != null) {
                if (!ConvertUtils.isEmpty(mList)) {
                    Logger.d(TAG, "NaviAddViaAdapter item click " + position + ",mList：" + mList.size());
                    if (mList.size() > 1 && (position != mList.size() - 1)) {
                        onItemClickListener.onItemClick(position, mList.get(position));
                        holder.itemBinding.swipeMenuLayout.smoothClose();
                    }
                }
            }
        });

        holder.itemBinding.llActionContainer.setOnClickListener(v -> {
            Logger.d(TAG, "NaviAddViaAdapter item click del " + position + ",mList：" + mList.size());
            holder.itemBinding.swipeMenuLayout.smoothClose();
            if (onItemClickListener != null) {
                onItemClickListener.onDelClick(position, mList.get(position));
            }
        });
    }

    @SuppressLint("SetTextI18n")
    private void setChargeUi(final int chargeLeft, final SkinImageView img,
                             final SkinTextView text) {
        final int leftCharge = Math.max(-99, chargeLeft);
        if (!ConvertUtils.isEmpty(leftCharge)) {
            //50%以上电量，显示满电量图片，20-50%电量，显示半电量图片
            //0-20电量，显示低电量图片，文本变红
            //小于0%电量，显示空电量图片，文本变红
            if (leftCharge >= 50 && leftCharge <= 100) {
                img.setImageResource(R.drawable.img_electricity_full_42);
                text.setTextColor(
                        ResourceUtils.Companion.getInstance().
                                getColor(R.color.poi_details_bottom_ff_00));
            } else if (leftCharge > 20 && leftCharge < 50) {
                img.setImageResource(R.drawable.img_electricity_medium_42);
                text.setTextColor(
                        ResourceUtils.Companion.getInstance().
                                getColor(R.color.poi_details_bottom_ff_00));
            } else if (leftCharge > 0 && leftCharge <= 20) {
                img.setImageResource(R.drawable.img_electricity_low_42);
                text.setTextColor(
                        ResourceUtils.Companion.getInstance().
                                getColor(R.color.navi_color_C73333_100));
            } else if (leftCharge <= 0) {
                img.setImageResource(R.drawable.img_electricity_empty_42);
                text.setTextColor(
                        ResourceUtils.Companion.getInstance().
                                getColor(R.color.navi_color_C73333_100));
            }
            text.setText(leftCharge + "%");
        }
    }

    @Override
    public int getItemCount() {
        Logger.d(TAG, "NaviAddViaAdapter getItemCount " + mList.size());
        return mList.size();
    }

    public static class ResultHolder extends RecyclerView.ViewHolder {
        public SceneNaviViaListItemBinding itemBinding;

        public ResultHolder(SceneNaviViaListItemBinding resultItemBinding) {
            super(resultItemBinding.getRoot());
            this.itemBinding = resultItemBinding;
            this.itemBinding.setHolder(this);
        }
    }
}