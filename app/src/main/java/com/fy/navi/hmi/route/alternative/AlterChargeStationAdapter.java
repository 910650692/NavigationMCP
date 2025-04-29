package com.fy.navi.hmi.route.alternative;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.AppCompatTextView;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.R;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.route.RouteAlterChargeStationInfo;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.view.SkinConstraintLayout;

import java.util.ArrayList;
import java.util.List;

/**
 * @author  LiuChang
 * @version  \$Revision.1.0\$
 * Date: 2025/3/6
 * Description: [替换充电站适配器]
 */
public class AlterChargeStationAdapter extends RecyclerView.Adapter<AlterChargeStationAdapter.AlterChargeStationViewHolder> {
    private List<RouteAlterChargeStationInfo> mData = new ArrayList<>();
    private ItemClickListener mListener;
    private Context mContext;

    public AlterChargeStationAdapter(final Context context, final List<RouteAlterChargeStationInfo> data) {
        this.mContext = context;
        this.mData = data;
    }

    /**
     * 设置数据
     *
     * @param data 数据
     */
    public void setData(final List<RouteAlterChargeStationInfo> data) {
        this.mData.clear();
        this.mData = data;
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public AlterChargeStationViewHolder onCreateViewHolder(final @NonNull ViewGroup parent, final int viewType) {
        final View view = LayoutInflater.from(mContext).inflate(R.layout.item_alter_charge_stations, parent, false);
        return new AlterChargeStationViewHolder(view);
    }

    @Override
    public void onBindViewHolder(final @NonNull AlterChargeStationViewHolder holder, @SuppressLint("RecyclerView") final int position) {
        holder.mTvName.setText(mData.get(position).getMName());
        holder.mTvNumber.setText(String.valueOf(position + 1));
        holder.mTvCost.setText(mContext.getString(R.string.route_charge_stations_cost,
                mData.get(position).getMPriceInfo().getMLowestPriceValue(), mData.get(position).getMPriceInfo().getMLowestPriceUnit()));

        String fastTotalNumber = mData.get(position).getMFastPlugInfo().getMTotalNumber();
        if (fastTotalNumber.isEmpty()) {
            fastTotalNumber = mContext.getString(R.string.route_invalid);
            holder.mLayoutFast.setVisibility(View.GONE);

        } else {
            holder.mLayoutSlow.setVisibility(View.VISIBLE);
        }
        holder.mTvFastTotalNumber.setText(fastTotalNumber);

        String slowTotalNumber = mData.get(position).getMSlowPlugInfo().getMTotalNumber();
        if (slowTotalNumber.isEmpty()) {
            slowTotalNumber = mContext.getString(R.string.route_invalid);
            holder.mLayoutSlow.setVisibility(View.GONE);
        } else {
            holder.mLayoutSlow.setVisibility(View.VISIBLE);
        }
        holder.mTvSlowTotalNumber.setText(slowTotalNumber);

        holder.mLayoutChargeStation.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View view) {
                if (mListener != null) {
                    mListener.onItemClick(mData.get(position).getMPoiId());
                }
            }
        });

        holder.mButtonAlter.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View view) {
                if (mListener != null) {
                    mListener.onAlterClick(mData.get(position));
                }
            }
        });

        SearchPackage.getInstance().getTravelTimeFuture(new GeoPoint(mData.get(position).getMPos().getLon(),
                        mData.get(position).getMPos().getLat()))
                .thenAccept(pair -> {
                    ThreadManager.getInstance().postUi(() -> {
                        holder.mTvDistance.setText(pair.first);
                    });
                })
                .exceptionally(error -> {
                    Logger.d("getTravelTimeFuture error:" + error);
                    return null;
                });
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public static class AlterChargeStationViewHolder extends RecyclerView.ViewHolder {
        private final AppCompatTextView mTvName;
        private final AppCompatTextView mTvNumber;
        private final AppCompatTextView mTvDistance;
        private final AppCompatTextView mTvFastTotalNumber;
        private final AppCompatTextView mTvSlowTotalNumber;
        private final AppCompatTextView mTvCost;
        private final AppCompatTextView mButtonAlter;
        private final SkinConstraintLayout mLayoutChargeStation;
        private final SkinConstraintLayout mLayoutFast;
        private final SkinConstraintLayout mLayoutSlow;


        public AlterChargeStationViewHolder(final @NonNull View itemView) {
            super(itemView);
            mTvNumber = itemView.findViewById(R.id.tv_charge_station_number);
            mTvName = itemView.findViewById(R.id.tv_charge_station_name);
            mTvDistance = itemView.findViewById(R.id.tv_charge_station_distance);
            mLayoutFast = itemView.findViewById(R.id.ly_charge_fast);
            mTvFastTotalNumber = itemView.findViewById(R.id.tv_charge_fast_total_number);
            mLayoutSlow = itemView.findViewById(R.id.ly_charge_slow);
            mTvSlowTotalNumber = itemView.findViewById(R.id.tv_charge_slow_total_number);
            mTvCost = itemView.findViewById(R.id.tv_charge_station_cost);
            mButtonAlter = itemView.findViewById(R.id.button_alter);
            mLayoutChargeStation = itemView.findViewById(R.id.layout_charge_station);
        }
    }

    public void setListener(final ItemClickListener listener) {
        mListener = listener;
    }

    public interface ItemClickListener {
        /**
         * 详情点击
         * @param poiID poiID
         */
        void onItemClick(final String poiID);

        /**
         * 替换按钮点击
         *  @param info 替换点信息
         */
        void onAlterClick(final RouteAlterChargeStationInfo info);
    }
}